import { defineConfig } from 'vite';
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  root: 'client/',
  plugins: [scalaJSPlugin()],
  server: {
    allowedHosts: ['5173-brennanyoun-ageofchesss-yg3w3aafb9u.ws-us117.gitpod.io'] // TODO change this
  }
});