# Contributing to __daapr__ development

__daapr__ project incorporates several sub-packages including  __`dpbuild`__,  __`dpdeploy`__, and __`dpi`__. You are encouraged to participate in the __daapr__ development and share your ideas to improve upon the current state of the project. You can do so by

1. Filing a bug report or feature request using GitHub issue [here](https://github.com/amashadihossein/daapr/issues).
2. Making a pull request [here](https://github.com/amashadihossein/daapr/pulls).

Note that by participating in this project, you agree to comply with this [Contributor Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html).

You can follow the following guidelines when filing an issue or making a pull request;

#### Filing an issue
When reporting a bug or problem, it is crucial to incorporate minimal and reproducible example of the problem so that the problem can be quickly identified and verified. For an issue to be reproducible, three essential components are to be provided: necessary packages, data, and code along with instructions on how to run the code.Below are few tips that you can utilize when filing an issue

1. Make sure you load required packages at the top off the script
2. Include sample non-sensitive data
3. Make sure the code is readable and concise.
4. Add comments to clarify the code steps and remove any unrelated codes

#### Making pull request

In order to contribute to __daapr__ project, these are the steps you should follow;

1. Fork or create a new branch out of the **dev** version of the package and make required changes
2. Push the branch to GitHub and make a pull request (PR)
3. Explain the requested changes in the PR
4. Repeat steps 1-3 until the PR is accepted or it is determined that the changes are suitable for __daapr__

When making the PR, make sure that your PR should clearly state the reason for the change and how it addresses
a problem. You should also provide a brief but clear explanation of how the proposed solution in the PR solves it succinctly.

Before submitting your PR, please double check that it only contains the intended changes, and no unrelated modifications are being requested to be pulled. This will make it easier for us to review the changes and evaluate any potential unintended consequences.

Please follow a consistent and uniform coding style throughout the code which makes it simpler to navigate the code.

When a new function or parameter/s is/are added to the package, please make sure to document them with `roxygen2` and run your code changes with `devtools::document()` before submitting the PR.

When adding new functionality, please consider a way to implement it with minimal impact on existing code.

#### Attribution
This Code of Conduct references content from the [Contributor Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html),
version 2.0,
available at <https://www.contributor-covenant.org/version/2/0/code_of_conduct.html>.

This document references content from the [contributing to tmle3 development](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html).
