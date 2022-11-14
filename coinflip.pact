(namespace "free")

(module coinflip GOV
  "Coinflip module"

  ;; Import `coin` module while only making the `details` function available
  ;; in the `coinflip` module body
  (use coin)

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (WITHDRAW))
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-ops:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (update m-guards OPS_GUARD
        { "guard": guard }  
      )

      "Rotated OPS to a new guard"
    )
  )

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun init-perms:string (gov:guard ops:guard)
    @doc "Initializes the guards and creates the tables for the module"

    ;; This is only vulnerable if GOV_GUARD doesn't exist
    ;; Which means it's only vulnerable if you don't call 
    ;; init when you deploy the contract.
    ;; So let us be sure that init is called. =)
    (insert m-guards GOV_GUARD
      { "guard": gov }  
    )
    (insert m-guards OPS_GUARD
      { "guard": ops }  
    )
  )

  ;; -------------------------------
  ;; Decimal Values

  (defschema decimal-value
    @doc "Stores decimal values"
    value:decimal
  )
  (deftable decimal-values:{decimal-value})

  (defun update-decimal-value (val-id:string value:decimal)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write decimal-values val-id
        { "value": value }
      )
    )
  )

  (defun get-decimal-value:decimal (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read decimal-values val-id ["value"]))
  )

  ;; -------------------------------
  ;; Int Values

  (defschema int-value
    @doc "Stores decimal values"
    value:integer
  )
  (deftable int-values:{int-value})

  (defun update-int-value:string (val-id:string value:integer)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write int-values val-id
        { "value": value }
      )
    )
  )

  (defun get-int-value:integer (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read int-values val-id ["value"]))
  )

  ;; -------------------------------
  ;; Constants

  (defconst SITE_FEE_KEY:string "SITE_FEE")
  (defconst WIN_CHANCE_KEY:string "WIN_CHANCE")

    
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

  
  (defun place-bet:string (account:string prediction:integer rand:integer amount:decimal)
    @doc "Start the betting."
    (with-capability (OPS)
      (coin.transfer account TREASURY_BANK amount)

      (if (= (mod rand (get-int-value WIN_CHANCE_KEY)) prediction)
        (let 
          (
            (exists (winner-exists account))
            (winAmount (* (- amount (* amount (get-decimal-value SITE_FEE_KEY))) 2))
          )
          (if exists 
            (with-read winners account {
                "wonCount":= wonCount,
                "amountKDA":= amountKDA
              }
              (update winners account {
                "wonCount":(+ wonCount 1),
                "amountKDA":(+ amountKDA winAmount)
              })
            )
            (insert winners account { 
                "account":account, 
                "amountKDA":winAmount,
                "wonCount":1
              }
            )
          )
        )
        "Lost bet"
      )
    )
  )

  (defun get-claim-amount:decimal (account:string)
    @doc "Get the claim amount by winner account"
    ;; Read the row using the account as key and select only amountKDA column
    (with-default-read winners account
      {"amountKDA":0}
      {"amountKDA":= amountKDA}
      amountKDA
    )
  )

  (defun get-treasury:decimal (balanceType:string)
    @doc "Get the balance of coinflip treasury"
    ;; Read the row using the balanceType as key and select only amountKDA column
    (with-default-read treasuries balanceType
      {"amountKDA":0}
      {"amountKDA":= amountKDA}
      amountKDA
    )
  )

  (defun winner-exists:bool (account:string)
    @doc "Check if the winner exists"
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
    @doc "Check if the winners balance exists"
    ;; Read from winners table using `account` param value as key.
    (with-default-read winners account
      {"amountKDA":0}
      {"amountKDA":= amountKDA}
      (>= amountKDA amount)
    )
  )

  (defcap WINNER (account:string amount:decimal)
    @doc "Make sure the requester owns the KDA account"
    ;; Get the guard of the given KDA account using coin.details function
    ;; and execute it using `enforce-guard`
    (enforce-guard (at 'guard (coin.details account)))
    (with-default-read winners account
      {"amountKDA":0, "wonCount":0}
      {"wonCount":= wonCount, "amountKDA":= amountKDA}
      (enforce (> wonCount 0) "You haven't won yet")
      (enforce (>= amountKDA amount) (format "Your winning balance is not enough for claim {}" [amount]))
    )
    (compose-capability (WITHDRAW))
  )

  (defun withdraw-winnings (account:string amount:decimal)
    @doc "Withdraw the winning balance"
    (with-capability (WINNER account amount)
      
      (install-capability (coin.TRANSFER TREASURY_BANK account amount))
      (coin.transfer TREASURY_BANK account amount)
          

      (with-read winners account {
          "amountKDA":= amountKDA
        }
        ( update winners account {
              "amountKDA": (- amountKDA amount)
        })
      )
    )
  )

  ;; -------------------------------
  ;; Bank Creation and Guard

  (defconst TREASURY_BANK:string (bank-account-name))

  (defcap WITHDRAW ()
    @doc "Used to give permission to withdraw money from the bank"
    true
  )

  (defun require-WITHDRAW:bool ()
    (require-capability (WITHDRAW))
    true
  )

  (defun bank-guard:guard ()
    @doc "Creates a guard that is used for the bank of the pool"
    (create-user-guard (require-WITHDRAW))
  )

  (defun bank-account-name:string ()
    (create-principal (bank-guard))
  )

  (defun withdraw-from-bank:string (receiver:string amount:decimal)
    @doc "Ops function that enables bonded NFT managers to withdraw from a pool's bank. \
    \ Expects that the receiver exists."
    (with-capability (OPS)
      (install-capability (coin.TRANSFER TREASURY_BANK receiver amount))
      (coin.transfer TREASURY_BANK receiver amount)
    )
  )

  (defun intilialize()
      (with-capability (OPS)
          (coin.create-account TREASURY_BANK (bank-guard))
          "Bank accounts have been created"
      )
  )
)



(if (read-msg "init")
  [
    (create-table m-guards)
    (create-table decimal-values)
    (create-table int-values)
    (create-table treasuries)
    (create-table winners)
    (init-perms (read-keyset "gov") (read-keyset "ops"))
    (update-decimal-value SITE_FEE_KEY 0.035)
    (update-int-value WIN_CHANCE_KEY 2)
    (intilialize)
  ]
  "Contract upgraded"
)