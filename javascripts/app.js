/*global console,$,XMLHttpRequest,document,ace,Handlebars,_*/
var app = function () {

    "use strict";
    var esURL = "172.16.12.162:9201",
        resultSize = 50,
        analyzedProjContainer = $("#analyzedProj"),
        resultTreeContainer = $("#resultTreeContainer"),
        resultTreeTemplateHTML = $("#result-tree-template").html(),
        resultTemplateHTML = $("#result-template").html(),
        resultTreeTemplate = Handlebars.compile(resultTreeTemplateHTML),
        resultTemplate = Handlebars.compile(resultTemplateHTML),
        Range = ace.require('ace/range').Range,
        errorElement = $("#connectionError"),
        leftPanel = $("#leftPanel"),
        rightSideContainer = $("#rightSideContainer"),
        expandIcon = $("#expand"),
        compressIcon = $("#compress"),
        errorMsgContainer = $("#errorMsg");

    function init() {
        resultTreeContainer.hide();
        compressIcon.hide();
    }

    init();

    function getMarkers(lineNumbers) {
        return lineNumbers.map(function (line) {
            /*var startMark = line - 6,
             endMark = line + 4;*/
            /*IMPORTANT NOTE: Range takes row number starting from 0*/
            return new Range(line - 1, 0, line - 1, 1000);
        });
    }

    function enableAceEditor(id, content, lineNumbers) {
        var editor = ace.edit(id),
            markers = getMarkers(lineNumbers);

        editor.resize(true);

        editor.setTheme("ace/theme/github");
        editor.getSession().setMode("ace/mode/java");

        editor.setReadOnly(true);
        editor.setValue(content, 1);

        markers.forEach(function (m) {
            editor.getSession().addMarker(m, "ace_selection", "background");
        });

        editor.gotoLine(lineNumbers[lineNumbers.length - 1], 0, true);
    }

    function getFileName(filePath) {
        var elements = filePath.split("/"),
            repoName = elements[0] + "-" + elements[1],
            fileName = elements[elements.length - 1];
        return {"repo": repoName, "file": fileName};
    }

    function updateLeftPanel(processedData) {
        var projects = [],
            groupedByRepos = _.groupBy(processedData, function (entry) {
                return entry.repo;
            });

        projects = _.map(groupedByRepos, function (files, label) {
            return {
                name: label,
                files: _.unique(files, _.iteratee('name'))
            }
        });

        analyzedProjContainer.hide();
        resultTreeContainer.show();
        resultTreeContainer.html(resultTreeTemplate({"projects": projects}));
    }

    function fetchFileQuery(fileName) {
        return {"query": {"term": {"typesourcefile.fileName": fileName}}}
    }

    function queryES(indexName, queryBody, resultSize, successCallback) {
        $.es.Client({
            host: esURL,
            log: 'trace'
        }).search({
            index: indexName,
            size: resultSize,
            body: queryBody
        }).then(function (result) {
                successCallback(result.hits.hits);
            }, function (err) {
                errorMsgContainer.text(err.message);
                errorElement.slideDown("slow");
                errorElement.slideUp(2500);
            }
        )
    }

    function updateRightSide(processedData) {
        var files = processedData.slice(0, 1);

        files.forEach(function (fileInfo, index) {

            queryES("sourcefile", fetchFileQuery(fileInfo.path), 1, function (result) {
                var id = "result" + index,
                    content = "";
                if (result.length > 0) {
                    content = result[0]._source.fileContent;
                    enableAceEditor(id + "-editor", content, fileInfo.lines);
                } else {
                    $("#" + id).hide();
                }
            });
        });

        $("#results").html(resultTemplate({"files": files}));
    }

    function filterRelevantTokens(searchString, tokens) {
        var result = searchString.split(",").map(function (term) {

            var matchingTokens = [],
                correctedTerm = term.trim().replace(/\*/g, ".*").replace(/\?/g, ".{1}");

            matchingTokens = tokens.filter(function (tk) {
                return (tk["importName"]).search(correctedTerm) >= 0;
            });

            return matchingTokens;
        });

        return _.flatten(result);
    }

    function buildSearchString(str) {
        var result = "";
        if (str[0] === "\'") {
            result = str.substr(1, str.length - 2);
        } else {
            result = str.split(",").map(function(entry){
                return "*"+entry.trim();
            }).join(",");
        }
        return result;
    }

    function processResult(searchString, data) {
        var result = [],
            intermediateResult = [],
            groupedData = [];

        groupedData = _.groupBy(data, function (entry) {
            return entry._source.file;
        });

        intermediateResult = _.map(groupedData, function (files, fileName) {
            var labels = getFileName(fileName),
                lineNumbers = [];

            files.forEach(function (f) {
                var matchingTokens = filterRelevantTokens(searchString, f._source.tokens);
                var possibleLines = _.pluck(matchingTokens, "lineNumbers");
                lineNumbers = lineNumbers.concat(possibleLines);
            });

            lineNumbers = (_.unique(_.flatten(lineNumbers))).sort(function (a, b) {
                return a - b;
            });

            return {path: fileName, repo: labels.repo, name: labels.file, lines: lineNumbers}

        });

        /* sort by descending lengths*/
        result = _.sortBy(intermediateResult, function (elem) {
            return -elem.lines.length;
        });

        return result;
    }

    function updateView(searchString, data) {
        var processedData = processResult(searchString, data);

        updateLeftPanel(processedData);
        updateRightSide(processedData);
    }

    function getQuery(queryString) {
        var terms = queryString.split(","),
            mustTerms = terms.map(function (queryTerm) {
                var prefix = (queryTerm.search(/\*/) >= 0 || queryTerm.search(/\?/) >= 0) ? "wildcard" : "term";
                var result = {};
                result[prefix] = {"custom.tokens.importName": queryTerm.trim()};
                return result;
            });

        return {
            "bool": {
                "must": mustTerms,
                "must_not": [],
                "should": []
            }
        };
    }

    function search(queryString) {
        var correctedQuery = buildSearchString(queryString),
            queryBlock = getQuery(correctedQuery);

        queryES("betterdocs", {
            "query": queryBlock,
            "sort": [
                {"score": {"order": "desc"}}]
        }, resultSize, function (result) {
            updateView(correctedQuery, result);
        });
    }

    function updateConfig(url, size) {
        esURL = url.trim();
        resultSize = size;
    }

    function expandResultView() {
        leftPanel.hide();
        expandIcon.hide();
        rightSideContainer.addClass("fullWidth");
        compressIcon.show();
    }

    function compressResultView() {
        leftPanel.show();
        compressIcon.hide();
        rightSideContainer.removeClass("fullWidth");
        expandIcon.show();
    }

    return {
        search: search,
        saveConfig: updateConfig,
        expand: expandResultView,
        compress: compressResultView
    };
}();