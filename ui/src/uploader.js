export function uploadImage(channelId) {
  var fileSelect = document.createElement('input');
  fileSelect.type = 'file';
  fileSelect.name = 'file'

	fileSelect.onchange = function(event) {
    event.preventDefault();
    uploadFile(fileSelect, channelId);
  };

	fileSelect.click();
}


function uploadFile(fileSelect, channelId) {
  var files = fileSelect.files;
  var formData = new FormData();

  for (var i = 0; i < files.length; i++) {
    var file = files[i];
    if (!file.type.match('image.*')) {
      continue;
    }
    formData.append('file', file, file.name);
  }

  
  var url = '/api/channel/upload/' + channelId
  var xhr = new XMLHttpRequest();

  xhr.open('POST', url, true);
  xhr.onload = function () {
    if (xhr.status === 200) {
      // console.log('done')
      location.reload();
    } else {
      console.log('error')
    }
  };

  xhr.send(formData);
}
