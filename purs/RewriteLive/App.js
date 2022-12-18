"use strict";

export const typeset = function () {
    console.log("RW: Asking Mathjax to re-typeset.");
    try {
        MathJax.typeset();
    } catch (error) {
        console.log("RW: MathJax.typeset() failed. Maybe MathJax loaded yet.")
        console.error(error);
    }
};
