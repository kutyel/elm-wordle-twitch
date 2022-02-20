import { Elm } from './Main.elm'

const app = Elm.Main.init({ node: document.getElementById('root') })

app.ports.copy.subscribe((sharedString) => {
  navigator.clipboard.writeText(sharedString)
})
