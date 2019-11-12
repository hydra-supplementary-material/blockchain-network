{- |

Assumed by the test infrastructure:

  * A delegation map is an injective function from genesis ("cold") keys to
    operational ("hot") keys, and no operational key is a genesis key.

  * The chain's ledger includes a delegation map.

  * The genesis chain's ledger delegation map is total, mapping every genesis
    key present in that genesis configuration.

  * A node's static configuration determines its operational key. A node
    operator can only "change" a node's operational key by restarting or
    replacing that node.

  * An honest node cannot forge a block if its operational key is not in the
    range of its current ledger's delegation map.

Guaranteed by the test infrastructure:

  * The test infrastructure maintains the *actual* delegation map.

  * The genesis chain's ledger delegation map equals the initial actual
    delegation map.

  * Each ledger delegation map is either synchronized with the actual
    delegation map or else out-of-date.

  * When the test infrastucture changes the actual delegation map, it also
    promptly generates the necessary delegation certificate transactions to
    re-synchronize the ledger delegation maps with the new actual delegation
    map.

  * Invariant: No two node instances simultaneously have the same operational
    key.

  * Invariant: For all genesis keys @gk@, no two node instances simultaneously
    have a current ledger that maps @gk@ to their own operational key.

Mechanism:

We ensure that last invariant by always shutting down the relevant node
instance before altering the actual delegation map.

* Suppose we are changing the actual delegate map so that it maps @gk@ to @ok2@
  instead of @ok1@.

* We would first shut down the node currently configured to use @ok1@, if any.

* We would then update the actual delegate map.

* We would then either restart the node or replace it with a new one; either
  way, the resulting node is (newly) configured to use @ok2@.

* We would generate the @gk := ok2@ delegate certificate transaction in the new
  node's memory pool. (TxSub will propagate this to all downstream nodes, so it
  can get into the ledger. The new (honest) node can't lead until that
  happens.)

Concern:

* What if the transaction expires before being propagated or else before being
  included in a block?

* Similar but slightly different: What if the transaction does get included in
  a block, but then we switch off that chain?

It seems like the new node may need to persistently re-generate that
transaction until it finds it in its immutable DB.

-}