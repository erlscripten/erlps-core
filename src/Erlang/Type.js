"use static";

exports.showArrayImplGeneral =
    function (left) {
	return function(right) {
	    return function(inter) {
		return function (f) {
		    return function (xs) {
			var ss = [];
			for (var i = 0, l = xs.length; i < l; i++) {
			    ss[i] = f(xs[i]);
		    }
			return left + ss.join(inter) + right;
		    };
		};
	    };
	};
    };
