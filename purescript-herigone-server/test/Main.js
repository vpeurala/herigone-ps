"use strict";

var originalConsoleLog = console.log;
var logs = [];

exports.turnOnConsoleLogCapturing = function() {
  console.log = function(s) {
    logs.push(s);
  };
};

exports.getCapturedConsoleLogs = function() {
  return logs;
};

exports.turnOffConsoleLogCapturing = function() {
  console.log = originalConsoleLog;
};
