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

function barcodeKeyPress( event ) { // takes input from either keyboard or barcode scanner
    var key = String.fromCharCode( event.charCode )
    var input = document.getElementById( 'crement' )
    var inputcount = document.querySelectorAll( '[type="text"]' ).length + document.querySelectorAll( '[type="number"]' ).length 
    if ( input && inputcount == 1 ) { // focus in 'crement' when no other input fields only
        input.focus()
    }
    var focused = document.activeElement
    if ( !focused || focused == document.body ) {
        focused = null
    } else if ( document.querySelector ) {
        focused = document.querySelector( ":focus" )
    }
    if (
        !event.ctrlKey && !event.altKey // no hotkeys used
        && ( focused == null || focused.tagName != "INPUT" ) // focus not in input fielf for manual input
    ) {
        if ( event.keyCode === 13 ) { // carriage return
            var input = document.getElementById( 'barcodeInput' )
            if ( input && barcodeBuf.length > 0 ) {
                input.setAttribute( 'value', barcodeBuf )
                input.parentNode.submit()
                return
            }
            barcodeBuf = ""
            event.preventDefault()
        } else if ( event.keyCode === 27 ) { // escape
            console.log( "escape" )
            barcodeBuf=hideBarcode()
            event.preventDefault()
        } else if ( event.keyCode === 8 ) { // backspace
            console.log( "backspace" )
            barcodeBuf = barcodeBuf.substring( 0, barcodeBuf.length - 1  )
            showBarcode( barcodeBuf )
            if ( barcodeBuf.length <= 0 ) {
                barcodeBuf = hideBarcode()
            }
            event.preventDefault()
        } else if ( event.keyCode == 0 && key != " " ) { // e.g. F-Keys are 112 to 123, A-Za-z0-9 all are 0.
            console.log( "some input: " + barcodeBuf + "[" + key + "] <= {" + event.keyCode + "}" )
            barcodeBuf += key
            showBarcode( barcodeBuf )
            event.preventDefault()
        }
    }
}
