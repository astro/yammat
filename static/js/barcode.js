var barcodeBuf = ""
var barcodeShown = false

function showBarcode(text) {
    if (!barcodeShown) {
        document.getElementById('barcode').classList.add('shown')
    }
    document.getElementById('barcodeContent').textContent = text
}

function hideBarcode() {
    if (barcodeShown) {
        document.getElementById('barcode').classList.remove('shown')
    }
    return document.getElementById('barcodeContent').textContent
}

function barcodeKeyPress(event) {
    var key = String.fromCharCode(event.charCode)
    var input = document.getElementById('crement')
    if ( input ) {
        input.focus()
    }
    var focused = document.activeElement
    if (! focused || focused == document.body) {
        focused = null
    } else if (document.querySelector) {
        focused = document.querySelector(":focus")
    }
    if ( focused == null || focused.tagName != "INPUT" ) {
        if ( event.keyCode === 13 ) {
            var input = document.getElementById('barcodeInput')
            if (input) {
                input.setAttribute('value', barcodeBuf)
                input.parentNode.submit()
                return
            }
            barcodeBuf = ""
            event.preventDefault()
        } else if ( event.keyCode === 27 ){
            barcodeBuf=hideBarcode()
            event.preventDefault()
        } else if ( event.keyCode === 9 ){
            barcodeBuf = barcodeBuf.substring( 0, barcodeBuf.length - 1  )
            if ( barcodeBuf.length <= 0 ) {
                barcodeBuf=hideBarcode()
            }
            event.preventDefault()
        } else {
            barcodeBuf += key
            showBarcode(barcodeBuf)
            event.preventDefault()
        }
    }
}
