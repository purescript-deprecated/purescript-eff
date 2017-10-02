"use strict";

exports.unsafePerformEff = function (f) {
  return f();
};
