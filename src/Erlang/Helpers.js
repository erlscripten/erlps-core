"use strict";

exports.falsifyErrorsImpl =
    function(false_val) {
        return function(action) {
            try {
                return action();
            } catch(_) {
                return false_val;
            }
        };
    };
