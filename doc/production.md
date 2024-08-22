<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Going to production with CTL](#going-to-production-with-ctl)
  - [Setup security](#setup-security)
  - [Achieving best performance](#achieving-best-performance)
  - [Balancing](#balancing)
  - [Monitoring](#monitoring)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Going to production with CTL

## Setup security

Ensure that:

- You have thoroughly tested your dApp. Your tests should be based on security properties you want to maintain (who should/shouldn't be able to do X? under what conditions?). Write these properties down and review them as well.
- There are no private keys in the bundles
- Your private keys are stored securely (e.g. not served by the webserver)
- If you have private keys that are used specifically to control/shut down the system in case of a disaster, you are not storing them on the same server as the app.
- Blockfrost keys are not exposed to the user. If you run with Blockfrost, you have to set up a reverse proxy that attaches the project ID header.

## Achieving best performance

Most likely, you will need a custom query layer to scale your dApp. Kupo is not very performant when it comes to handling a lot of queries on mainnet if it is configured to index the whole blockchain: [it has a slightly different purpose](https://github.com/CardanoSolutions/kupo/issues/146#issuecomment-1810571796).

The best approach would be to maintain your dApp state by folding incoming transactions (while handling rollbacks). Consider using [Ogmios chain-sync API](https://ogmios.dev/mini-protocols/local-chain-sync/).

[Here's how to plug your own query layer as a "provider" into CTL](./custom-query-layers.md).

## Balancing

Use multiple backend instances if you have a lot of users, but avoid switching between different instances for a single user: CTL mildly relies on consistency of the state.

How many instances to run? Compute the number based on the response times of your query layer and expected number of users.

## Monitoring

Set up monitoring and alerts for your backend services.

Ensure that you actively check the following:

- Disk space
- RAM usage
- Rate of failed requests to the query layer backend (e.g. kupo returns 503 when overloaded)
- Average response time
- Usage limits for Blockfrost
