package com.github.arena.challenges.weakmdparser;

import java.util.Objects;

public class MarkdownParser {

    public String parse(final String markdown) {

        var lines = markdown.split("\n");
        var resultHtml = new StringBuilder();
        var isActiveList = false;

        for (String lineOfMarkdown : lines) {

            var parsedLine = parseHeader(lineOfMarkdown);

            if (Objects.isNull(parsedLine)) {
                parsedLine = parseListItem(lineOfMarkdown);
            }

            if (Objects.isNull(parsedLine)) {
                parsedLine = parseParagraph(lineOfMarkdown);
            }

            if (isUnorderedList(isActiveList, parsedLine)) {

                isActiveList = true;
                resultHtml.append("<ul>");
                resultHtml.append(parsedLine);

            } else if (Boolean.FALSE.equals(isListElement(parsedLine))) {

                closeUlTag(isActiveList, resultHtml);
                isActiveList = false;
                resultHtml.append(parsedLine);

            } else {
                resultHtml.append(parsedLine);
            }
        }

        closeUlTag(isActiveList, resultHtml);

        return resultHtml.toString();
    }
    private void closeUlTag(final Boolean isToClose, final StringBuilder resultHtml) {

        if (Boolean.TRUE.equals(isToClose)) {
            resultHtml.append("</ul>");
        }
    }

    private boolean isUnorderedList(final boolean activeList, final String theLine) {
        return isListElement(theLine) && !isHeaderElement(theLine) && !isParagraphElement(theLine) && !activeList;
    }

    private Boolean isParagraphElement(final String theLine) {
        return theLine.matches("(<p>).*");
    }

    private Boolean isHeaderElement(final String theLine) {
        return theLine.matches("(<h).*");
    }

    private Boolean isListElement(final String theLine) {
        return theLine.matches("(<li>).*");
    }

    private String parseHeader(final String markdown) {

        final var hashCount = countHashAmount(markdown);
        return Objects.isNull(hashCount) ? null : "<h" + hashCount + ">" + markdown.substring(hashCount + 1) + "</h" + hashCount + ">";
    }

    private Integer countHashAmount(final String markdown) {
        int hashCount = 0;

        for (int i = 0; i < markdown.length() && markdown.charAt(i) == '#'; i++) {
            hashCount++;
        }

        if (hashCount == 0) {
            return null;
        }
        return hashCount;
    }

    private String parseListItem(final String markdown) {

        if (markdown.startsWith("*")) {
            var skipAsterisk = markdown.substring(2);
            var listItemString = parseTextStyles(skipAsterisk);
            return "<li>" + listItemString + "</li>";
        }
        return null;
    }

    private String parseParagraph(final String markdown) {
        return "<p>" + parseTextStyles(markdown) + "</p>";
    }

    private String parseTextStyles(final String markdown) {
        var workingOn = parseBold(markdown);
        return parseItalic(workingOn);
    }

    private String parseBold(final String markdown) {
        var boldRegex = "__(.+)__";
        var replacement = "<strong>$1</strong>";
        return markdown.replaceAll(boldRegex, replacement);
    }

    private String parseItalic(final String workingOn) {
        var italicRegex = "_(.+)_";
        var replacement = "<em>$1</em>";
        return workingOn.replaceAll(italicRegex, replacement);
    }
}
