// Generated by purs version 0.11.7
"use strict";
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var Prelude = require("../Prelude");
var MonadFork = function (Monad0, fork) {
    this.Monad0 = Monad0;
    this.fork = fork;
};
var monadForkAff = new MonadFork(function () {
    return Control_Monad_Aff.monadAff;
}, function ($3) {
    return Data_Functor.map(Control_Monad_Aff.functorAff)(Control_Monad_Aff.cancel)(Control_Monad_Aff.forkAff($3));
});
var fork = function (dict) {
    return dict.fork;
};
var monadForkReaderT = function (dictMonadFork) {
    return new MonadFork(function () {
        return Control_Monad_Reader_Trans.monadReaderT(dictMonadFork.Monad0());
    }, function (v) {
        return function (r) {
            return Data_Functor.map((((dictMonadFork.Monad0()).Bind1()).Apply0()).Functor0())(Data_Functor.map(Data_Functor.functorFn)(Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(dictMonadFork.Monad0())))(fork(dictMonadFork)(v(r)));
        };
    });
};
module.exports = {
    fork: fork,
    MonadFork: MonadFork,
    monadForkAff: monadForkAff,
    monadForkReaderT: monadForkReaderT
};
