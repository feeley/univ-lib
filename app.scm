;;; File: app.scm

;;;----------------------------------------------------------------------------

(declare (standard-bindings) (extended-bindings) (not safe))

#|
(##inline-host-declaration #<<host-code-end

function js_setTimeout(thunk, timeout) { setTimeout(thunk, timeout); }
function js_alert(text) { alert(text); }

function js_load_js(url) {
  var scr = document.createElement('script');
  scr.setAttribute('type', 'text/javascript');
  scr.setAttribute('src', url);
  document.getElementsByTagName('head').item(0).appendChild(scr);
}



function XHR_send(callback, retry, url, data) {

    var req = false;

    function new_req() {
        if (window.XMLHttpRequest) { // Mozilla, Safari, ...
            req = new XMLHttpRequest();
            if (req.overrideMimeType) {
                req.overrideMimeType('text/xml');
            }
        } else if (window.ActiveXObject) { // IE
            try {
                req = new ActiveXObject('Msxml2.XMLHTTP');
            } catch (e) {
                try {
                    req = new ActiveXObject('Microsoft.XMLHTTP');
                } catch (e) {}
            }
        }
    }

    function init_req() {
        req.onreadystatechange = response_handler;
        req.open('POST', url, true);
        req.setRequestHeader('Content-type', 'text/plain');
        req.setRequestHeader('enctype', 'text/plain');
    }

    function send_req() {
        req.send(data);
    }

    function response_handler() {
        if (req.readyState === 4) {
            if (req.status === 200) {
                callback(req.responseText);
            } else if (retry) {
                add('retry','|r='+r+'|');
                setTimeout(send_req, 5000);
            } else {
                callback(null);
            }
        }
    }

    new_req();

    if (req !== false) {
        init_req();
        send_req();
    }

    return req;
}

function port_open(cont, specs) {
    XHR_send(cont,
             true,
             '/port_open',
             specs);
}
   
function port_read(cont, port_id, len) {
    XHR_send(cont,
             true,
             '/port_read?port_id=' + port_id + '&' + 'len=' + len,
             null);
}

function port_write(cont, port_id, data) {
    XHR_send(cont,
             true,
             '/port_write?port_id=' + port_id,
             data);
}

function port_close(cont, port_id) {
    XHR_send(cont,
             true,
             '/port_close?port_id=' + port_id,
             null);
}




function get(id) {
    return document.getElementById(id).innerHTML;
}

function set(id, content) {
    document.getElementById(id).innerHTML = content;
}

function add(id, content) {
    set(id, get(id) + content);
}

function setup() {

        function cont2(port_id) {

            function cont3(str) {

                function cont4(r) {
                    add('output','|cont4 r='+r+'|');
                    cont2(port_id);
                }

                function cont5(r) {
                    add('output','|cont5 r='+r+'|');
                    //setup();
                }

                add('output','|cont3 str='+str+'|');
                if (str === '')
                    port_close(cont5, port_id);
                else
                    cont2(port_id);//port_write(cont4, port_id, str);
            }

            add('output','|cont2 port_id='+port_id+'|');
            port_read(cont3, port_id, 500);
        }

        add('output','|port_open|');
        port_open(cont2,
//                  '(open-file path: "README" direction: input)'
//                  '(open-process path: "ls" arguments: ("../../..") direction: input)'
//                  '(open-directory path: "../../..")'
                  '(open-tcp-server port-number: 12345)'
);
}

function js_go() {
    setTimeout(setup,1000);
}

host-code-end
)

(define setTimeout (##inline-host-expression "gambit_js2scm(js_setTimeout)"))
(define alert (##inline-host-expression "gambit_js2scm(js_alert)"))
(define load-js (##inline-host-expression "gambit_js2scm(js_load_js)"))

(define go (##inline-host-expression "gambit_js2scm(js_go)"))

(go)
|#


(declare (safe))

(println (thread-name (make-thread (lambda () (current-thread)))))
(println (thread-name (make-thread (lambda () (current-thread)) 'allo)))

(println (mutex-name (make-mutex)))
(println (mutex-name (make-mutex 'allo)))

(println (condition-variable-name (make-condition-variable)))
(println (condition-variable-name (make-condition-variable 'allo)))

(##trap
 (lambda ()
   (eval '(println (quote a)))))

(##trap
 (lambda ()
;   (println (foo 1 2 3))
   (println (eval '(bar 1 2 3)))))

