/* eslint-disable */

export var postSessionLogin = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/session/login', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/session/register', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/session/logout', true);
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
  xhr.open('GET', 'https://api.kleanstudio.com/booth/session/user', true);
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
  xhr.open('GET', 'https://api.kleanstudio.com/booth/session/users', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/session/user' + '?userToken=' + encodeURIComponent(userToken), true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/session/validate', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/service/charge', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/service/day', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/service/month', true);
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
  xhr.open('POST', 'https://api.kleanstudio.com/booth/service', true);
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

export var deleteService = function(token, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', 'https://api.kleanstudio.com/booth/service' + '?token=' + encodeURIComponent(token), true);
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

export var putServiceByToken = function(token, body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.kleanstudio.com/booth/service/' + encodeURIComponent(token) + '', true);
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

export var putServiceProcessingByToken = function(token, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.kleanstudio.com/booth/service/processing/' + encodeURIComponent(token) + '', true);
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

export var postPaymentIntent = function(headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/payment/intent', true);
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

export var postPaymentFeeIntent = function(body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/payment/fee/intent', true);
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

export var postPaymentFeeIntentByToken = function(token, body, headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/payment/fee/intent/' + encodeURIComponent(token) + '', true);
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

export var getPaymentCharge = function(headerAuthorization, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/payment/charge', true);
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

export var postHooksPayment_intent = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/hooks/payment_intent', true);
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

export var getAdminValidate = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/admin/validate', true);
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

export var getAdminSearch = function(name, email, date, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/admin/search' + '?name=' + encodeURIComponent(name) + '&email=' + encodeURIComponent(email) + '&date=' + encodeURIComponent(date), true);
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

export var putAdminDenyByDeny = function(deny, body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.kleanstudio.com/booth/admin/deny/' + encodeURIComponent(deny) + '', true);
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

export var getAdminRates = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/admin/rates', true);
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

export var getAdminRatesFee = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/admin/rates/fee', true);
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

export var putAdminRates = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.kleanstudio.com/booth/admin/rates', true);
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

export var postAdminProjects = function(body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'https://api.kleanstudio.com/booth/admin/projects', true);
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

export var putAdminProjectsByProject = function(project, body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', 'https://api.kleanstudio.com/booth/admin/projects/' + encodeURIComponent(project) + '', true);
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

export var deleteAdminProjectsByProject = function(project, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', 'https://api.kleanstudio.com/booth/admin/projects/' + encodeURIComponent(project) + '', true);
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

export var getProjectsProjects = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'https://api.kleanstudio.com/booth/projects/projects', true);
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
