var barcodeBuf = ""
var barcodeShown = false

function showBarcode(text) {
    if (!barcodeShown) {
        document.getElementById('barcode').classList.add('shown')
        barcodeShown = true
    }
    document.getElementById('barcodeContent').textContent = text
}

function hideBarcode() {
    if (barcodeShown) {
        document.getElementById('barcode').classList.remove('shown')
        barcodeShown = false
    }
    return ""
}

function barcodeKeyPress(event) {
    var key = String.fromCharCode(event.charCode)
    var input = document.getElementById('crement')
    var inputcount = document.querySelectorAll('[type="text"]').length + document.querySelectorAll('[type="number"]').length 
    if ( input && inputcount == 1 ) {
        input.focus()
    }
    var focused = document.activeElement
    if ( !focused || focused == document.body ) {
        focused = null
    } else if ( document.querySelector ) {
        focused = document.querySelector(":focus")
    }
    if ( inputcount <= 1 && ( focused == null || focused.tagName != "INPUT" ) ) {
        if ( event.keyCode === 13 ) {
            var input = document.getElementById('barcodeInput')
            if (input && barcodeBuf.length > 0) {
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
