# E2E Testing in the Browser

CTL comes with advanced machinery for E2E testing in the browser, which can be used to either run the included examples (in `examples`) or create a custom test suite for E2E testing.

- [E2E Testing in the Browser](#e2e-testing-in-the-browser)
- [Parts Involved](#parts-involved)
- [How to Run the Included Examples](#how-to-run-the-included-examples)
- [Accepted Command Line Options](#accepted-command-line-options)
- [How Wallets are Used](#how-wallets-are-used)
  - [How to Use a Different Version of a Wallet](#how-to-use-a-different-version-of-a-wallet)
  - [Where to Find the Installed Extensions](#where-to-find-the-installed-extensions)
  - [Re-Package an Extension as a CRX File](#re-package-an-extension-as-a-crx-file)
  - [Use a CRX File](#use-a-crx-file)
  - [How to Use a Different User Wallet](#how-to-use-a-different-user-wallet)
- [How to Create Your Own Test Suite](#how-to-create-your-own-test-suite)
- [Using a reproducible `chromium` version](#using-a-reproducible-chromium-version)

## Parts Involved

[Puppeteer](https://github.com/puppeteer/puppeteer) (driven by [Toppokki](https://github.com/justinwoo/purescript-toppokki))
is used to drive the tests. Supported browsers are [Chromium](https://www.chromium.org/) and Google Chrome.
The browser can be run headless (default) or headful (useful during test development).

Any programs that should be tested must be deployed and running on some testserver (e.g. for the included examples we use `make run-dev`). The test suite accepts a list of URLs.

An executable for particular tests is also needed. For a working example see `test/E2E.purs`. It can be run conveniently using `./test/ctl-e2e-test.sh`.

## How to Run the Included Examples

The process is as follows:

1. Set `ps-entrypoint` in Makefile to `Examples.ByURL`.
2. run `make run-dev`.
3. In another shell, run `./test/ctl-e2e-test.sh run`.
4. Examples will be run headless by default (pass `--no-headless` to inspect the browser window). In case of errors, the browser console output will be printed to the shell.

## Accepted Command Line Options

The provided test suite accepts some options. Run `./test/ctl-e2e-test.sh` for help. `run` command passes provided argument to the PS program. For a complete explanation, see `src/Contract/Test/E2E/Browser.purs`.

## How Wallets are Used

For purposes of testing, there are two parts to using a wallet: providing the right software version and importing a wallet with enough assets and a known password.

- The software just needs to be unpacked to some directory. This can either be the location where the browser unpacks it, or the result of unpacking a CRX file (see below).
- We provide the wallet data as a single tarball which will be unpacked into the chrome profile before a test run.

### How to Use a Different Version of a Wallet

Chrome extensions are unpacked to some directory by the browser. From there, they can either be used directly by the tests (which gives no control over upgrades and instead uses always the current version), or they can be repackaged as CRX files. The default setup provides CRX versions which the script automatically unpacks on each test run.

The default test suite accepts the arguments of form `--nami-dir`, `--gero-dir`, etc., to point to the directories from which the extensions are loaded. In order to use the "live" version of an extension, just pass the arguments accordingly, e.g.:

```
@spago test --main Test.E2E -a "E2ETest --nami-dir ~/.config/google-chrome/Default/Extensions/lpfcbjknijpeeillifnkikgncikgfhdo/ --gero-dir ~/.config/google-chrome/Default/Extensions/iifeegfcfhlhhnilhfoeihllenamcfgc --chrome-exe google-chome
```

### Where to Find the Installed Extensions

1. Locate your browser profile directory. Commonly used locations include: `~/.config/{google-chrome,chromium}/Default` (where `Default` is the profile name), `~/snap/chromium/common/chromium/Default`.
2. Make sure that inside the profile, your desired extension is unpacked. Nami should be in `Extensions/lpfcbjknijpeeillifnkikgncikgfhdo`, Gero (testnet version) in `Extensions/iifeegfcfhlhhnilhfoeihllenamcfgc`.
3. Add the version as a subdirectory, too. The final path may look like `/home/user/.config/google-chrome/Default/Extensions/iifeegfcfhlhhnilhfoeihllenamcfgc/1.10.9_0`

### Re-Package an Extension as a CRX File

1. Make sure your browser is using the desired extension version.
2. Navigate to chrome://extensions/
3. Click the extension.
4. Switch on "Developer mode" (upper right corner).
5. Click "Pack extension".
6. Paste the extension's directory (see above) into "Extension root directory". You can leave "Private key file" empty.
7. Click "Pack extension".
8. The path of the CRX file is displayed in the browser.

(See [puppeteer-crx](https://www.npmjs.com/package/puppeteer-crx) for an effort to automate this process.)

### Use a CRX File

We use `unzip` to unpack it. However, `unzip` will issue a warning because of extra bytes at the beginning, and will exit with a non-zero code, so the exit code needs to be ignored. (we use `|| echo to achieve that`).

### How to Use a Different User Wallet

In the test suite, the wallet settings are just unpacked using `tar`.

A new settings tarball can be easily created:

1. Run the browser with `./test/ctl-e2e-test.sh browser`
2. Set up a wallet extension, create and fund the wallet, and set the collateral.
3. Close the browser.
4. Run the script with `pack` argument. The file located at `test-data/settings.tar.gz` will be updated.

## How to Create Your Own Test Suite

If you are using CTL as a library, you can and should create your own test suite to test your own contracts.

1. Take `test/E2E.purs` as inspiration and create your own binary. You will find the necessary machinery in `Contract.Test.E2E`. Notable components:
   - `withBrowser`: bracket to launch the browser with a specific extension, run something and clos the browser.
   - `parseOptions`: Parses command line options, in case you want to use the same as our example suite.
   - `publishTestFeedback`, `resetTestFeedback`, `retrieveTestFeedback`: Can be used to communicate success or failure from a contract to the tests.
   - `geroConfirmAccess`, `geroSign`, `namiConfirmAccess`, `namiSign`: Confirm a transaction in the browser (i.e. enter the password, click "Sign")
   - `withExample`: navigate to a URL, detect the wallet and get ready to run a contract.
2. Fire up your own contracts.
3. Take the `Makefile` as an inspiration, prepare your wallets and run the tests.

## Using a reproducible `chromium` version

Although most users will have some version of Chromium or Google Chrome installed system-wide, it can be a good idea to use the same version for all e2e testing. When creating your project's `devShell` using `purescriptProject`, you can set the `shell.withChromium` flag to `true` to include it in the shell's packages. This will be the version of `chromium` present in the `nixpkgs` you pass to create your project:

```nix
{
  projectFor = system:
    let
      pkgs = nixpkgsFor system;
    in
    pkgs.purescriptProject {
      inherit pkgs;
      projectName = "my-project";
      shell = {
        withChromium = true;
        # ...
      };
      # ...
    };
}
```

## Using custom unauthorized extensions

If an extension is not published on Chrome Web Store, it may not have a stable ID. Look into `manifest.json` `key` property to check - if it is not present, the ID will change on each reload, which makes automation impossible.

To freeze the ID, first unpack the `.crx` archive, load the extension unpacked (in developer mode), and then pack it back. It will give you a new `.crx` as well as a `.pem` file with the same name. The ID can now be derived from this key using this command:

```
openssl rsa -in key.pem -pubout -outform DER | openssl base64 -A
```

Unarchive the CRX, put the encoded public key to `key` property of `manifest.json` file, and archive the directory back. You can then reload the browser multiple times and the ID will remain the same.

[More on extension IDS](https://stackoverflow.com/questions/37317779/making-a-unique-extension-id-and-key-for-chrome-extension)
