// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef __bigExplore_RcppExports_h__
#define __bigExplore_RcppExports_h__

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "../inst/include/bigExplore.h"

namespace bigExplore {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("bigExplore", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("bigExplore", "bigExplore_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in bigExplore");
            }
        }
    }

    inline void biglmResidCpp(Rcpp::XPtr<BigMatrix> pBigMat, Rcpp::XPtr<BigMatrix> pRes, Rcpp::XPtr<BigMatrix> pResid, arma::colvec coeff) {
        typedef SEXP(*Ptr_biglmResidCpp)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_biglmResidCpp p_biglmResidCpp = NULL;
        if (p_biglmResidCpp == NULL) {
            validateSignature("void(*biglmResidCpp)(Rcpp::XPtr<BigMatrix>,Rcpp::XPtr<BigMatrix>,Rcpp::XPtr<BigMatrix>,arma::colvec)");
            p_biglmResidCpp = (Ptr_biglmResidCpp)R_GetCCallable("bigExplore", "bigExplore_biglmResidCpp");
        }
        RNGScope __rngScope;
        RObject __result = p_biglmResidCpp(Rcpp::wrap(pBigMat), Rcpp::wrap(pRes), Rcpp::wrap(pResid), Rcpp::wrap(coeff));
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<void >(__result);
    }

}

#endif // __bigExplore_RcppExports_h__