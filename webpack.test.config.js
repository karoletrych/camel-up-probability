var path = require("path");
var webpack = require("webpack");

module.exports = {
    mode: "development",
    entry: "./test/Test.fsproj",
    output: {
        path: path.join(__dirname, "./test/bin"),
        filename: "bundle.js",
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    },
    plugins: [
        new webpack.NamedModulesPlugin()
    ]
}
