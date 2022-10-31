;; coinflip.pact

;; Define a keyset with name `coinflip-admin-keyset`.
;; Keysets cannot be created in code, thus we read them in from the load message data.
(namespace "free")

(define-keyset "free.coinflip-admin-keyset" (read-keyset "coinflip-admin-keyset"))

;; Define `coinflip` module
(module coinflip GOVERNANCE
  "Coinflip module"

    (defcap GOVERNANCE ()
        "Module governance capability that only allows the admin to update this module"
        ;; Check if the tx was signed with the provided keyset, fail if not
        (enforce-keyset 'coinflip-admin-keyset))
    
    ;; Import `coin` module while only making the `details` function available
    ;; in the `election` module body
    (use coin [ details transfer])
    
    (defcap ACCOUNT-OWNER (account:string)
        "Make sure the requester owns the KDA account"
        ;; Get the guard of the given KDA account using coin.details function
        ;; and execute it using `enforce-guard`
        (enforce-guard (at 'guard (coin.details account)))
    )

    (defconst treasuryAccount:string "k:900ee4c3c0dd495c270897ccbc1d1c83b88db09d1a981f414d6cf5028d212d8b")
    
    (defschema bet
      account: string
      amountKDA: decimal
      prediction:integer
    )
    
    (defschema winner
      account:string
      amountKDA:decimal
      wonCount: integer
    )

    (defschema treasury
        balanceType:string
        amountKDA:decimal
    )
    
    (deftable bets:{bet})
    (deftable winners:{winner})
    (deftable treasuries:{treasury})

    (defun place-bet (account:string prediction:integer amount:decimal)
        "Start the betting."
        (with-capability (ACCOUNT-OWNER account)
            (transferProtected account treasuryAccount amount)
        )
    )

    (defun get-claim-amount:decimal (account:string)
        "Get the claim amount by winner account"
        ;; Read the row using the account as key and select only amountKDA column
        (at 'amountKDA (read winners account ['amountKDA]))
    )

    (defun get-treasury:decimal (balanceType:string)
        "Get the balance of coinflip treasury"
        ;; Read the row using the balanceType as key and select only amountKDA column
        (at 'amountKDA (read treasuries balanceType ['amountKDA]))
    )

    (defun winner-exists:bool (account:string)
        "Check if the winner exists"
        ;; Read from winners table using `account` param value as key.
        ;; with-default-read allows us to set default values for the table columns
        ;; That are returned if the row does not exist.
        (with-default-read winners account
            {"wonCount":0}
            {"wonCount":= wonCount}
            (> wonCount 0)
        )
    )

    (defun balance-exists:bool (account:string amount:decimal)
        "Check if the winners balance exists"
        ;; Read from winners table using `account` param value as key.
        (with-default-read winners account
            {"amountKDA":0}
            {"amountKDA":= amountKDA}
            (>= amountKDA amount)
        )
    )

    (defun withdraw-winnings (account:string, amount:decimal)
        "Withdraw the winning balance"
        (let ((exists (winner-exists account)))
            (enforce (= exists true) "Winner doesn't exist"))

        (let ((exists (balance-exists account amount)))
            (enforce (= exists true) "Your winning balance is not enough for claim {}" [amount]))
        
        ;; Try to acquire the `ACCOUNT-OWNER` capability which checks.
        ;; that the transaction owner is also the owner of the KDA account provided as parameter to our `withdraw` function.
        (with-capability (ACCOUNT-OWNER account)
            (transferProtected treasuryAccount account amount)
        )
    )

    (defun transferProtected (from:string to:string amount:decimal)
        (coin.transfer from to amount)
    )

)