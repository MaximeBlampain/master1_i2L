const { DataTypes } = require('sequelize');
const { sequelize } = require('../utils/mysql');

const Users = sequelize.define(
    'Users', 
    {
        // Model attributes
        id: {
            type: DataTypes.INTEGER,
            allowNull: false,
            primaryKey: true,
            autoIncrement: true,
            validate: {
                max: 100,
            },
            field: 'id_user'
        },
        firstName: {
            type: DataTypes.STRING,
            allowNull: false,
            validate: {
                max: 255,
            },
            field: 'first_name'
        },
        lastName: {
            type: DataTypes.STRING,
            allowNull: false,
            validate: {
                max: 255,
            },
            field: 'last_name'
        },
        email: {
            type: DataTypes.STRING,
            allowNull: false,
            validate: {
                max: 255,
            },
            field: 'email'
        },
        password: {
            type: DataTypes.STRING,
            allowNull: false,
            validate: {
                max: 255,
            },
            field: 'hash_password'
        },
        provider: {
            type: DataTypes.STRING,
            allowNull: false,
            validate: {
                max: 255,
            },
            field: 'provider'
        },
        providerID: {
            type: DataTypes.STRING,
            allowNull: true,
            field: 'provider_id'
        }
    },
    {
        timestamps: false,
        tableName: 'users'
    }
);

sequelize.sync();
module.exports = Users