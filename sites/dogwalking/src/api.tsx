/* eslint-disable */

export var postSessionLogin = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/session/login', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postSessionRegister = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/session/register', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postSessionLogout = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/session/logout', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getSessionUser = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.treywilkinson.com/dogwalking/session/user', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getSessionUsers = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.treywilkinson.com/dogwalking/session/users', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(null);
};

export var postSessionUser = function(userToken, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/session/user' + '?userToken=' + encodeURIComponent(userToken), true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(null);
};

export var postSessionValidate = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/session/validate', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postServiceCharge = function(body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/service/charge', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postServiceDay = function(body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/service/day', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postServiceMonth = function(body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/service/month', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postService = function(body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.treywilkinson.com/dogwalking/service', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var putServiceByToken = function(token, body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.treywilkinson.com/dogwalking/service/' + encodeURIComponent(token) + '', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var deleteServiceByToken = function(token, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', 'https://api.treywilkinson.com/dogwalking/service/' + encodeURIComponent(token) + '', true);
  xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.withCredentials = true;
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res !== undefined && res !== null)  onError(res);
      }
    }
  };
  xhr.send(null);
};
