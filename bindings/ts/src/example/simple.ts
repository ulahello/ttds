import { Pane } from "../mod";

function sleep(ms: number) {
    return new Promise((res) => setTimeout(res, ms))
}

for (let i = 0; i < 10; i++) {
    const pane = await Pane.init({ color: "#ffffff", baseURL: "https://ttds.tali.network" });

    pane.draw({ type: 'rect', w: 500, h: 500, x: 20, y: 40, color: "#dddddd" })

    setTimeout(async () => {
        await pane.delete()
    }, 5000)

    await sleep(1200)
}