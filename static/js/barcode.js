var barcodeBuf = ""
var barcodeShown = false

function showBarcode(text) {
    if (!barcodeShown) {
        document.getElementById('barcode').classList.add('shown')
    }
    document.getElementById('barcodeContent').textContent = text
}

function barcodeKeyPress(event) {
    var key = String.fromCharCode(event.charCode)
    if (event.keyCode === 13) {
        var uid = document.getElementById('uid').getAttribute('value')
        if (uid) {
            window.location = "/user/" + encodeURIComponent(uid) + "/barcode/" + encodeURIComponent(barcodeBuf)
        }
        barcodeBuf = ""
        event.preventDefault()
    } else {
        barcodeBuf += key
        showBarcode(barcodeBuf)
        event.preventDefault()
    }
}
