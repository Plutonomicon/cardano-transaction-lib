const { createInteractionContext, createStateQueryClient } = require('@cardano-ogmios/client')

getContext = connConfig => () => {
  return new Promise((res, rej) => {
    createInteractionContext(
      err => { console.log("ogmios error: ", err); rej(err)},
      success => {console.log('getcontext succeeded'); res(success) 
      }, 
      connConfig)
  })
}
