# ctl-scaffold

Welcome to your new CTL project!

This template presents the way the CTL library can be used to build a web-based application.

The application consists of two parts:
* The core part of the application in the `src` folder
that implements the logic in PureScript using the CTL library.
* The API module in the `api` folder is written in TypeScript
and provides access to the core component for the UI (demo).

Both components compile into one bundle that can be used at your discern.

A small React-based web application in the `demo` folder showcases it.

## Running the demo

### Developing and bundling the application

Nix is needed to work on the project and build the bundle.
Once the bundle is built you can use the NPM package in `templates/ctl-scaffold`
within `demo` (or your own application) without Nix.

To start working enter the Nix environment by running `nix develop` in the `templates/ctl-scaffold` folder.
Please make sure to use Nix v2.8 or later.

Familiarize yourself with files in `src` and `api` folders.
In the case you are planning to run real queries
put in proper a Blockfrost key in `src/Api.purs` line 23.

To build the application and bundle it run

```bash
$ make bundle
```

Once done you will see `./dist` folder containing both application components. 
Every time you made a change in `src` and/or `api` folders
you need to rebuild with this command.

Explore other targets in the `Makefile` which includes `format` and others.

### Running the demo

Now the folder `templates/ctl-scaffold` is an NPM package.
Our next step will be to use it within the `demo` web page.
The current setup brings the component using `file:..`
(see `demo/packages.json`) but you can publish it if it's needed.

Open another terminal in `templates/ctl-scaffold/demo` folder. Ensure you have `npm` on your path.

> Make sure not to be in the Nix shell since the command won't work well in that case.

Step first is to install all dependencies into local `demo/node_modules`. 
Check that this folder indeed exists once it is done.
There is no need to re-run this command after bundling the application
since `file:` schema places a symbolic link inside `node_modules` folder.

```bash
$ npm install
```

Now we are ready to serve the page with

```bash
$ npm run serve
```

Open a new tab in your browser open the console and point it to
`http://localhost:8080/index.html`

Explore the `src` folder to see how the application is used on the page. 
Every time you change either the application or the demo page you have to serve it again with that command.

## Further steps

Please also visit our:

- [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

- [PureScript API documentation for CTL](https://plutonomicon.github.io/cardano-transaction-lib/)

- [Discord server](https://discord.gg/JhbexnV9Pc)

If you encounter problems and/or want to report a bug,
you can open an issue [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues).

Please search for existing issues beforehand!
