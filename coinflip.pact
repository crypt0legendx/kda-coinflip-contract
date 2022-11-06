;; coinflip.pact

;; Define a keyset with name `coinflip-admin-keyset`.
;; Keysets cannot be created in code, thus we read them in from the load message data.
(namespace "free")

(define-keyset "free.coinflip-admin-keyset4" (read-keyset "coinflip-admin-keyset4"))

;; Define `coinflip` module
(module coinflip4 GOVERNANCE
  "Coinflip module"

    (defcap GOVERNANCE ()
        "Module governance capability that only allows the admin to update this module"
        ;; Check if the tx was signed with the provided keyset, fail if not
        (enforce-keyset 'coinflip-admin-keyset4))
    
    ;; Import `coin` module while only making the `details` function available
    ;; in the `coinflip` module body
    (use coin)
    
    (defcap ACCOUNT-OWNER (account:string)
        "Make sure the requester owns the KDA account"
        ;; Get the guard of the given KDA account using coin.details function
        ;; and execute it using `enforce-guard`
        (enforce-guard (at 'guard (coin.details account)))
    )

    (defconst siteFee:decimal 0.035)
    (defconst TREASURY-BANK:string (create-principal (treasury-bank-guard)))

    (defun treasury-bank-guard:guard ()
        (create-module-guard "treasury"))

    (defun get-bank ()
          TREASURY-BANK)

    
    (defschema winner
      account:string
      amountKDA:decimal
      wonCount:integer
    )

    (defschema treasury
        balanceType:string
        amountKDA:decimal
    )
    
    (deftable winners:{winner})
    (deftable treasuries:{treasury})

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

    (defun generateRandom:integer ()
        (mod (at 'block-height (chain-data)) 2)
    )

    
    (defun place-bet (account:string prediction:integer amount:decimal)
        "Start the betting."
        
        (coin.transfer account TREASURY-BANK amount)
        (let ((randomiss (generateRandom)))
            (if (= randomiss prediction)
                (let ((exists (winner-exists account))(winAmount (* (- amount (* amount siteFee)) 2)))
                    (if (= exists true) 
                        (with-read winners account {
                            "wonCount":= wonCount,
                            "amountKDA":= amountKDA
                           }
                          ( update winners account {
                                "wonCount":(+ wonCount 1),
                                "amountKDA":(+ amountKDA winAmount)
                          }))
                        (insert winners account { 
                                "account":account, 
                                "amountKDA":winAmount,
                                "wonCount":1
                            }
                        )
                    )
                )
            )
        )
    )

    (defun get-claim-amount:decimal (account:string)
        "Get the claim amount by winner account"
        ;; Read the row using the account as key and select only amountKDA column
        (with-default-read winners account
            {"amountKDA":0}
            {"amountKDA":= amountKDA}
            (amountKDA)
        )
    )

    (defun get-treasury:decimal (balanceType:string)
        "Get the balance of coinflip treasury"
        ;; Read the row using the balanceType as key and select only amountKDA column
        (with-default-read treasuries balanceType
            {"amountKDA":0}
            {"amountKDA":= amountKDA}
            (amountKDA)
        )
    )

    (defun withdraw-winnings (account:string amount:decimal)
        "Withdraw the winning balance"
        (let ((exists (winner-exists account)))
            (enforce (= exists true) "Winner doesn't exist"))

        (let ((exists (balance-exists account amount)))
            (enforce (= exists true) "Your winning balance is not enough for claim {}" [amount]))
        
        (coin.transfer TREASURY-BANK account amount)

        (with-read winners account {
            "amountKDA":= amountKDA
           }
          ( update winners account {
                "amountKDA":(- amountKDA amount)
          }))
        
    )

    (defun intilialize()
        (with-capability (GOVERNANCE)
            (coin.create-account TREASURY-BANK (treasury-bank-guard))
            "Bank accounts have been created"
        )
    )
)

(create-table winners)
(create-table treasuries)