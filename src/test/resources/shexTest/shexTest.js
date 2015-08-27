var ret = {};
['schemas', 'negativeSyntax', 'illDefined', 'parsedSchemas', 'validations', "ASTs"].forEach(function (dir) {
  ret[dir] = __dirname + '/' + dir + '/';
});

if (typeof require !== 'undefined' && typeof exports !== 'undefined')
  module.exports = ret;
else
  shexTest = ret;

