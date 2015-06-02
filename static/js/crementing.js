/*
this is thought for (de-/in)crement integer values in (e.g. input) form elements
calling like
  <input type="input" value="++" onclick="crmnt(document.getElementById('id'),1)" />
  <input type="input" value="+=5" onclick="crmnt(document.getElementById('id'),5)" />
  <input type="input" value="-=5" onclick="crmnt(document.getElementById('id'),-5)" />
  <input type="input" value="--" onclick="crmnt(document.getElementById('id'),-1)" />
*/
function crmnt( e, i ) { /* read: in-/decrement ( dom-element, signed-integer ) */
  var d = parseInt( e.value ) + i;
  e.value = ( d <= 0 || isNaN(d) )? 0 : d;
}
