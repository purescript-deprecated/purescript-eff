"use strict";

// Eff a
// = { () -> a }
// | { () -> a, tag: "PURE",   _0 :: a,         _1 :: Void  }
// | { () -> a, tag: "MAP",    _0 :: b -> a,    _1 :: Ef b }
// | { () -> a, tag: "APPLY",  _0 :: Ef b,      _1 :: Ef (b -> a) }
// | { () -> a, tag: "BIND",   _0 :: b -> Ef a, _1 :: Ef b }

// Operation a b
// = { tag: "MAP",        _0 :: a -> b }
// | { tag: "APPLY",      _0 :: Ef a }
// | { tag: "APPLY_FUNC", _0 :: a -> b }
// | { tag: "BIND",       _0 :: a -> Ef b }

var PURE = "PURE";
var MAP = "MAP";
var APPLY = "APPLY";
var BIND = "BIND";
var APPLY_FUNC = "APPLY_FUNC";

var runEff = function (inputEff) {
  var operations = [];
  var eff = inputEff;
  var res;
  var op;
  var tag;
  effLoop: for (;;) {
    tag = eff.tag;
    if (tag !== undefined) {
      if (tag === MAP || tag === BIND || tag === APPLY) {
        operations.push(eff);
        eff = eff._1;
        continue;
      }
      // here `tag === PURE`
      res = eff._0;
    } else {
      res = eff();
    }

    while ((op = operations.pop())) {
      if (op.tag === MAP) {
        res = op._0(res);
      } else if (op.tag === APPLY_FUNC) {
        res = op._0(res);
      } else if (op.tag === APPLY) {
        eff = op._0;
        operations.push({ tag: APPLY_FUNC, _0: res });
        continue effLoop;
      } else { // op.tag === BIND
        eff = op._0(res);
        continue effLoop;
      }
    }
    return res;
  }
};

var mkEff = function (tag, _0, _1) {
  var eff = function eff_() { return runEff(eff_); };
  eff.tag = tag;
  eff._0 = _0;
  eff._1 = _1;
  return eff;
};

exports.pureE = function (x) {
  return mkEff(PURE, x);
};

exports.mapE = function (f) {
  return function (eff) {
    return mkEff(MAP, f, eff);
  };
};

exports.applyE = function (effF) {
  return function (eff) {
    return mkEff(APPLY, eff, effF);
  };
};

exports.bindE = function (eff) {
  return function (f) {
    return mkEff(BIND, f, eff);
  };
};

exports.runPure = function (f) {
  return f();
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};
