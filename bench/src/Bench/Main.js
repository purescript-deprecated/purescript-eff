"use strict";

exports.mkArr = function(){
  return { count: 0 };
};

exports.pushToArr = function(xs) {
  return function(x) {
    return function() {
      xs.count += 1
      return xs;
    };
  };
};

exports.log = function(x) {
  return function(){
    console.log(x)
  }
};