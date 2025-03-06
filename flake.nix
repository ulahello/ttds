{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.11";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
      pkgs.mkShell.override { stdenv = pkgs.clangStdenv; } {
        buildInputs = with pkgs; [
          systemd.dev meson ninja libdrm pkg-config

          cabal-install ghc zlib

          just
        ];

        CC = "clang";
        QEMU_NET_OPTS = "hostfwd=tcp::8080-:80";
      };

    packages.x86_64-linux.display =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
      (import ./display/nix { inherit pkgs; })
        .override { stdenv = pkgs.clangStdenv; };

    packages.x86_64-linux.web =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
      (import ./web { inherit pkgs; });

    packages.x86_64-linux.test-vm =
      self.nixosConfigurations.test-vm.config.system.build.vm;

    nixosModules.everything =
      { config, ... }: {
        networking.firewall.allowedTCPPorts = [ 80 ];
        systemd.services.ttds-runner = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "Run the ttds web server, wrapping the display server.";
          serviceConfig = {
            ExecStart = "${self.packages.x86_64-linux.web}/bin/ttds-web ${self.packages.x86_64-linux.display}/bin/ttds";
            WorkingDirectory = "/etc";
          };
        };

        services.nginx.enable = true;
        services.nginx.config = ''
	  events {}

	  http {
            limit_req_zone $binary_remote_addr zone=api:10m rate=20r/s;

            server {
              listen 80;

              location / {
                add_header Access-Control-Allow-Origin *;

                limit_req zone=api burst=50 nodelay;
                proxy_pass http://localhost:8080/;
              }
            }
	  }
        '';

        # Literal string "pass"
        environment.etc."admin.pass".text = "14|8|1|RfLpAkDgsS08AnUlq+ZAnytfSJ+HmHYpx1/rZKSxjCo=|8SoSVn8LCPhwy7ShTJDCeX8YDzD+5ecdodmqYWWzm68HVtmxrPOkJqT5ltwQpoUVF4Zdfh1ynKF9f5DLtJQMkA==\n";
      };

    nixosConfigurations.test-vm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { system.stateVersion = "24.11"; }
	{
	  users.users.admin = {
	    isNormalUser = true;
	    password = "admin";
	    extraGroups = [ "wheel" ];
	  };
	}
	self.nixosModules.everything
      ];
    };
  };
}
