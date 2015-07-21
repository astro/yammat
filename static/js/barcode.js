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
    var focused = document.activeElement
    if (! focused || focused == document.body) {
        focused = null
    } else if (document.querySelector) {
        focused = document.querySelector(":focus")
    }
    if (focused == null || focused.tagName != "INPUT") {
        if (event.keyCode === 13) {
            var input = document.getElementById('barcodeInput')
            if (input) {
                input.setAttribute('value', barcodeBuf)
                input.parentNode.submit()
                return
            }
            barcodeBuf = ""
            event.preventDefault()
        } else {
            barcodeBuf += key
            showBarcode(barcodeBuf)
            event.preventDefault()
        }
    }
}
