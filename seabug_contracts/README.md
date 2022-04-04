# Seabug contracts JS API

_This is a provisional project setup and will be subject to change_

Temporarily it is assumed that the development/deployment of the frontend takes place
inside CTL's nix environment. You can find the information on how to use it in `../README.md`
Extend it with necessary dependencies and place your web app js code so that its entrypoint
is found in `../frontend/index.js`.

You can include Seabug contracts JS API by requiring `./api.js` module.
To bundle the application call: `npm run bundle-seabug`

To work, an application using the API will need several services up and running. See (`../README.md`)
