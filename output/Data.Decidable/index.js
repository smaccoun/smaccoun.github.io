// Generated by purs version 0.11.7
"use strict";
var Control_Category = require("../Control.Category");
var Data_Comparison = require("../Data.Comparison");
var Data_Decide = require("../Data.Decide");
var Data_Divisible = require("../Data.Divisible");
var Data_Equivalence = require("../Data.Equivalence");
var Data_Monoid = require("../Data.Monoid");
var Data_Op = require("../Data.Op");
var Data_Predicate = require("../Data.Predicate");
var Data_Void = require("../Data.Void");
var Prelude = require("../Prelude");
var Decidable = function (Decide0, Divisible1, lose) {
    this.Decide0 = Decide0;
    this.Divisible1 = Divisible1;
    this.lose = lose;
};
var lose = function (dict) {
    return dict.lose;
};
var lost = function (dictDecidable) {
    return lose(dictDecidable)(Control_Category.id(Control_Category.categoryFn));
};
var decidablePredicate = new Decidable(function () {
    return Data_Decide.choosePredicate;
}, function () {
    return Data_Divisible.divisiblePredicate;
}, function (f) {
    return function (a) {
        return Data_Void.absurd(f(a));
    };
});
var decidableOp = function (dictMonoid) {
    return new Decidable(function () {
        return Data_Decide.chooseOp(dictMonoid.Semigroup0());
    }, function () {
        return Data_Divisible.divisibleOp(dictMonoid);
    }, function (f) {
        return function (a) {
            return Data_Void.absurd(f(a));
        };
    });
};
var decidableEquivalence = new Decidable(function () {
    return Data_Decide.chooseEquivalence;
}, function () {
    return Data_Divisible.divisibleEquivalence;
}, function (f) {
    return function (a) {
        return Data_Void.absurd(f(a));
    };
});
var decidableComparison = new Decidable(function () {
    return Data_Decide.chooseComparison;
}, function () {
    return Data_Divisible.divisibleComparison;
}, function (f) {
    return function (a) {
        return function (v) {
            return Data_Void.absurd(f(a));
        };
    };
});
module.exports = {
    lose: lose,
    Decidable: Decidable,
    lost: lost,
    decidableComparison: decidableComparison,
    decidableEquivalence: decidableEquivalence,
    decidablePredicate: decidablePredicate,
    decidableOp: decidableOp
};
