'use strict';

const knex = require('knex');

var KnexFile = require('../../config/knexfile');
// console.log("KnexFile: ", KnexFile);

var knexDbConfigs = {};
knexDbConfigs['development'] = KnexFile['development'];
knexDbConfigs['production'] = KnexFile['production'];

var dbEnvVar = 'MyChange_DbEnv';

// if (process.env[dbEnvVar] !== undefined) {
  var dbEnv = process.env[dbEnvVar] || 'development';
  console.log("MyChange_DbEnv: " + dbEnv);

  var currentDbConfig = knexDbConfigs[dbEnv];
  var configuredKnex = knex(currentDbConfig);

  // module.exports = myChangeDb;
// } else {
//   throw new Error('Environment variable: ' + dbEnvVar + ' MUST exist!');
// }


// exports.allUsersQuery = knex('users').select('*').orderBy('first_name', 'desc');

exports.allUsersQuery = configuredKnex('users').select('*').toSQL().sql;
