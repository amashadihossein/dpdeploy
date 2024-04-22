# dpdeploy 0.2.0

* Add back support for LabKey boards (#29). `pinsLabkey` is now required to work with LabKey boards

# dpdeploy 0.1.0

## Breaking changes

* dpdeploy now requires pins >= v1.2.0. This means that data products will now use the v1 api and older data products are incompatible with dpdeploy >= 0.1.0. Quite a few changes under the hood, but users will see minimal changes to the workflow. 
* LabKey functionality has been temporarily removed until pins v1 can be extended to support LabKey boards
* data products are now retrieved by pin hash, rather than version. Since pins v1 pin version has included both hash and datetime stamp. 

## Other improvments

* Added a `NEWS.md` file to track changes to the package.
* `dpconnect_check` removed to streamline workflow
