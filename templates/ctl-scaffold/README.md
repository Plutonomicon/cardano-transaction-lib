# ctl-scaffold

Welcome to your new CTL project!

This template presents the way CTL library can be used to build a web-based application.

The application consists of two parts:
* The core part of the application in `src` folder
that implements the logic in PureScript using CTL library.
* The API module written in TypeScript that provides
the access to the core component to be used by the UI
in `api` folder.

Both components compile into one bundle that can be used at your discern.

A small React-based web application in `demo` folder showcases it.

## Running the demo

### Developing and bundling the application

Nix is needed to work on the project and build the bundle.
Once the bundle is built you can use the NPM package in `templates/ctl-scaffold`
within `demo` (or your own application) without Nix.

To start working enter the Nix environment by running `nix develop` in the `templates/ctl-scaffold` folder.
Please make sure to use Nix v2.8 or later.

Familirize yourself with files in `src` and `api` folders.
In the case you are planning ro tun real queries
put in proper Blockfrost key in `src/Api.purs` line 23.

To build to application and bundle it run:

```bash
$ make bundle
```

Once done you will see `./dist` folder containig both application components. Every time you made a change in `src` and/or
`api` folders you need to rebuild with this command.

Explore other targets in the `Makefile`
which includes `format` and others.

### Using within demo

Now the folder `templates/ctl-scaffold` is a NPM package.
Our next step will be use it within the `demo` web-page.
The current setup bring the component using `file:..`
(see `demo/packages.json`) but you can publish it if it's needed.

Open another terminal in `templates/ctl-scaffold/demo` folder. Ensure you have `npm` on you path.

> Make sure not being in the Nix shell since command won't work well.

Step first is to install all dependencies into local `demo/node_modules`. Check that this folder indeed exists once it is done.
There is no need to re-run this command
after bundling the application since `file:` schema
places a symbolic link.

```bash
$ npm install
```

Now we are ready to serve the page with:

```bash
$ npm run serve
```

Open a new tab in your browser open the console and point it to
`http://localhost:8080/index.html`

Explore `src` folder to see how the application is used in the page. Every time you change either application or the demo page you have to serve it again.

## Further steps

Please also visit our:

- [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

- [PureScript API documentation for CTL](https://plutonomicon.github.io/cardano-transaction-lib/)

- [Discord server](https://discord.gg/JhbexnV9Pc)

If you encounter problems and/or want to report a bug,
you can open an issue [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues).

Please search for existing issues beforehand!