# Coinflip

This is the coinflip contract, recreated and fully tested.

In order to place a bet, you must call the `place-bet` function with the `OPS` capability.

Which means that the steps you have to go through to place a bet for a user are the following:

1. Send the user info to your backend
2. Construct the command with both the `TRANSFER` and `OPS` capabilities for the user, include your own random number from your backend
3. Sign the same command using your `OPS` key
4. Send the command back to the user and have them sign it
5. Send the transaction to the blockchain
