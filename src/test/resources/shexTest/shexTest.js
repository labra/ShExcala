var ret = {};
['schemas', 'negativeSyntax', 'parsedSchemas'].forEach(function (dir) {
  ret[dir] = __dirname + '/' + dir + '/';
});

if (typeof require !== 'undefined' && typeof exports !== 'undefined')
  module.exports = ret;
else
  shexTest = ret;

