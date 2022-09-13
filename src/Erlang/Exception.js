"use strict";

export const raise = function(ex) {
    throw ex;
};

export const getStack = function() {
    return new Error().stack;
};


export const tryCatch =
    function(exprC) {
        return function(handler) {
            try {
                let result = exprC();
                return result;
            } catch(error) {
                //console.log("gggg")
                //console.log(typeof(error))
                let resultEr = handler(error);
                return resultEr;
            }
        };
    };

export const tryOfCatch =
    function(exprC) {
        return function(ofHandler) {
            return function(handler) {
                var computed;
                try { computed = exprC(); }
                catch(error) {
                    //console.log("gggg")
                    //console.log(typeof(error))
                    let resultEr = handler(error);
                    return resultEr;
                }
                let result = ofHandler(computed);
                return result;
            };
        };
    };


export const tryCatchFinally =
    function(exprC) {
        return function(handler) {
            return function(afterC) {
                try {
                    let result = tryCatch(exprC)(handler);
                    return result;
                } finally { afterC(); }
            };
        };
    };

export const tryOfCatchFinally =
    function(exprC) {
        return function(ofHandler) {
            return function(handler) {
                return function(afterC) {
                    var computed;
                    try {
                        let result = tryOfCatch(exprC)(ofHandler)(handler);
                        return result;
                    } finally { afterC(); }
                };
            };
        };
    };
