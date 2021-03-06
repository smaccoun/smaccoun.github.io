// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var form = function ($0) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._form($0));
};
module.exports = {
    form: form,
    autofocus: $foreign.autofocus,
    setAutofocus: $foreign.setAutofocus,
    disabled: $foreign.disabled,
    setDisabled: $foreign.setDisabled,
    multiple: $foreign.multiple,
    setMultiple: $foreign.setMultiple,
    name: $foreign.name,
    setName: $foreign.setName,
    required: $foreign.required,
    setRequired: $foreign.setRequired,
    size: $foreign.size,
    setSize: $foreign.setSize,
    type_: $foreign.type_,
    length: $foreign.length,
    setLength: $foreign.setLength,
    selectedOptions: $foreign.selectedOptions,
    selectedIndex: $foreign.selectedIndex,
    setSelectedIndex: $foreign.setSelectedIndex,
    value: $foreign.value,
    setValue: $foreign.setValue,
    willValidate: $foreign.willValidate,
    validity: $foreign.validity,
    validationMessage: $foreign.validationMessage,
    checkValidity: $foreign.checkValidity,
    setCustomValidity: $foreign.setCustomValidity,
    labels: $foreign.labels
};
