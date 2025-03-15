import { Pane } from "../mod";

const pane = await Pane.init({ color: "#ffffff", baseURL: "https://example.com" });

pane.draw({ type: 'rectangle', w: 500, h: 500, x: 20, y: 40, color: "#dddddd" })

setTimeout(async () => {
    await pane.delete()
}, 5000)
