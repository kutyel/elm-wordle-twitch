import { Elm } from "./Main.elm";

const app = Elm.Main.init({ node: document.getElementById("root") });

app.ports.copy.subscribe((sharedString) => {
  console.log(sharedString);
  navigator.clipboard.writeText(sharedString);
});
