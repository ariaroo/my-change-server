'use strict';

var jwt = require('jsonwebtoken');


exports.verify = function (token) {
  return function (jwtConfigSecret) {
    return function (successCallback) {
      return function () {
        jwt.verify(token, jwtConfigSecret, function (err, decoded) {
          // console.log('decoded: ', decoded);

          if (!err) {
            successCallback(decoded);
          }
        });
      }
    };
  }
};
