# -- PKH --
function getPublicKeyHash () {
    cardano-cli address key-hash \
        --payment-verification-key-file $1 \
        --out-file public-key.hash
    echo $(cat public-key.hash)
    rm public-key.hash
}
# -- TxID --
function printTxIdInfo () {
    TxID=$(cardano-cli transaction txid --tx-file $1)
    echo "-----------------------------------"
    echo "Transaction ID : $TxID"
    echo "Cardanoscan    : https://preview.cardanoscan.io/transaction/$TxID"
    echo "============================================================================================================================="
}
# END -------------------------------------------------------------------------------------------------- #