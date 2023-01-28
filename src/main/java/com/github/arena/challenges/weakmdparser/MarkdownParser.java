package com.github.arena.challenges.weakmdparser;

public class MarkdownParser {

    String parse(final String markdown) {

        String[] lines = markdown.split("\n");
        StringBuilder resultHtml = new StringBuilder();
        boolean isActiveList = false;

        for (String lineOfMarkdown : lines) {

            String parsedLine = parseHeader(lineOfMarkdown);
            
            if (parsedLine == null) {
                parsedLine = parseListItem(lineOfMarkdown);
            }

            if (parsedLine == null) {
                parsedLine = parseParagraph(lineOfMarkdown);
            }

            if (isUnorderedList(isActiveList, parsedLine)) {
                isActiveList = true;
                resultHtml.append("<ul>");
                resultHtml.append(parsedLine);
            } else if (!isListElement(parsedLine) && isActiveList) {
                isActiveList = false;
                resultHtml.append("</ul>");
                resultHtml.append(parsedLine);
            } else {
                resultHtml.append(parsedLine);
            }
        }

        if (isActiveList) {
            resultHtml.append("</ul>");
        }

        return resultHtml.toString();
    }

    private static boolean isUnorderedList(final boolean activeList, final String theLine) {
        return isListElement(theLine) && !isHeaderElement(theLine) && !isParagraphElement(theLine) && !activeList;
    }

    private static boolean isParagraphElement(final String theLine) {
        return theLine.matches("(<p>).*");
    }

    private static boolean isHeaderElement(final String theLine) {
        return theLine.matches("(<h).*");
    }

    private static boolean isListElement(final String theLine) {
        return theLine.matches("(<li>).*");
    }

    protected String parseHeader(final String markdown) {
        int hashCount = 0;

        for (int i = 0; i < markdown.length() && markdown.charAt(i) == '#'; i++) {
            hashCount++;
        }

        if (hashCount == 0) {
            return null;
        }

        return "<h" + hashCount + ">" + markdown.substring(hashCount + 1) + "</h" + hashCount + ">";
    }

    public String parseListItem(final String markdown) {
        if (markdown.startsWith("*")) {
            String skipAsterisk = markdown.substring(2);
            String listItemString = parseSomeSymbols(skipAsterisk);
            return "<li>" + listItemString + "</li>";
        }
        return null;
    }

    public String parseParagraph(final String markdown) {
        return "<p>" + parseSomeSymbols(markdown) + "</p>";
    }

    public String parseSomeSymbols(final String markdown) {
        String workingOn = parseBold(markdown);
        return parseItalic(workingOn);
    }

    private static String parseBold(final String markdown) {
        String boldRegex = "__(.+)__";
        String replacement = "<strong>$1</strong>";
        return markdown.replaceAll(boldRegex, replacement);
    }

    private static String parseItalic(final String workingOn) {
        String italicRegex = "_(.+)_";
        String replacement = "<em>$1</em>";
        return workingOn.replaceAll(italicRegex, replacement);
    }
}
