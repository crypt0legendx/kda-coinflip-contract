# Coinflip

This is the coinflip contract, recreated and fully tested.

In order to place a bet, you must call the `place-bet` function with the `OPS` capability.

Which means that the steps you have to go through to place a bet for a user are the following:

1. Send the user info to your backend
2. Construct the command with both the `TRANSFER` and `OPS` capabilities for the user, include your own random number from your backend
3. Sign the same command using your `OPS` key
4. Send the command back to the user and have them sign it
5. Send the transaction to the blockchain

The above can still be hacked: People can get transactions from the backend and test them to see if they win. This can be mitigated by logging in, but still hackable.  
You will have to find a way to make it not hackable.  
You could likely use `defpacts` for the process: Someone places a bet, your backend supplies the random number in a second step.

For the TRANSFER cap, please see [this](https://tinyurl.com/ryosuke-transfer).  
You can also refer to line 42 of `coinflip.repl` for an example of capabilities.  

Built by Sterling Long!  
Luzzotica#6933  
k:aeecd476ad8a4842ec84f3fbdad39b73fe7329fb4feaa3ea4367314a29a7e42b  
