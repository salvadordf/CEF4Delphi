function myAlertFunction() {
    alert('This alert dialog is declared in a local JS file.');
}

function sendCustomReq() {
	var xhr = new XMLHttpRequest();
	var url = 'hello://localhost/customrequest?name1=value1';
	xhr.open('GET', url, true);
	xhr.onreadystatechange = function() {
		if(xhr.readyState == 4 && xhr.status == 200) {
			alert(xhr.responseText);
		}
	}	
	xhr.send();
}
