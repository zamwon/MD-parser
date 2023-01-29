package com.github.arena.challenges.weakmdparser;

import java.util.Objects;

public class MarkdownParser {

    public static final String NEW_LINE = "\n";
    public static final String OPEN_UL_TAG = "<ul>";
    public static final String CLOSE_UL_TAG = "</ul>";
    public static final String OPEN_LI_TAG = "<li>";
    public static final String CLOSE_LI_TAG = "</li>";
    public static final String OPEN_P_TAG = "<p>";
    public static final String CLOSE_P_TAG = "</p>";


    public String parse(final String markdown) {
        var lines = markdown.split(NEW_LINE);
        var resultHtml = new StringBuilder();
        var isToClose = false;

        for (String lineOfMarkdown : lines) {
            var parsedLine = parseHeader(lineOfMarkdown);

            if (isUlElement(parsedLine, isToClose)) {
                openUlTag(resultHtml);
                isToClose = true;

            } else if (Boolean.FALSE.equals(isListElement(parsedLine) && isToClose)) {
                closeUlTag(isToClose, resultHtml);
                isToClose = false;

            }
            appendParsedLine(resultHtml, parsedLine);
        }

        closeUlTag(isToClose, resultHtml);

        return resultHtml.toString();
    }

    private void appendParsedLine(StringBuilder resultHtml, String parsedLine) {
        resultHtml.append(parsedLine);
    }

    private static void openUlTag(final StringBuilder resultHtml) {
        resultHtml.append(OPEN_UL_TAG);
    }

    private void closeUlTag(final Boolean isToClose, final StringBuilder resultHtml) {
        if (Boolean.TRUE.equals(isToClose)) {
            resultHtml.append(CLOSE_UL_TAG);
        }
    }

    private boolean isUlElement(final String theLine, final boolean activeList) {
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
        if (Objects.isNull(hashCount)) {
            return parseListItem(markdown);
        } else {
            var openingHeaderTag = "<h" + hashCount + ">";
            var closingHeaderTag = "</h" + hashCount + ">";
            return openingHeaderTag + markdown.substring(hashCount + 1) + closingHeaderTag;
        }
    }

    private Integer countHashAmount(final String markdown) {
        int hashCount = 0;

        for (int i = 0; i < markdown.length() && markdown.charAt(i) == '#'; i++) {
            hashCount++;
        }

        return hashCount == 0 ? null : hashCount;
    }

    private String parseListItem(final String markdown) {
        if (markdown.startsWith("*")) {
            var skipAsterisk = markdown.substring(2);
            var listItemString = parseTextStyles(skipAsterisk);
            return OPEN_LI_TAG+ listItemString + CLOSE_LI_TAG;
        }
        return parseParagraph(markdown);
    }

    private String parseParagraph(final String markdown) {
        return OPEN_P_TAG + parseTextStyles(markdown) + CLOSE_P_TAG;
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
