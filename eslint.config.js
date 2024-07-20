import js from "@eslint/js";
import globals from "globals";

export default [
  js.configs.recommended,
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.commonjs,
        ...globals.es2021,
        ...globals.node
      },

      ecmaVersion: "latest",
      sourceType: "module"
    },

    rules: {
      "no-var": 2,
      "no-case-declarations": 0,

      "no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
          vars: "local"
        }
      ]
    }
  }
];
