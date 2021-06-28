<https://LiaScript.github.io/course/?https://github.com/EliasTechnik/EnglishPresentation/blob/main/imagebug.md>

# Bug: image does not load with underscore in Alt-Text

The markdown file and the picture is located here:
https://github.com/EliasTechnik/EnglishPresentation/blob/0f3ee2b2c775c5247add321834ff166430d836b3/imagebug.md (Markdown)
https://github.com/EliasTechnik/EnglishPresentation/blob/296de23b0fccd000b12f582b174769fdfb84f200/img/lo_res/imagebug/ttally_controller.jpg (Picture)

## Bug

The following image has one underscore in the alt-text, one in the path and one in the filename.

![ttally_controler class](img/lo_res/imagebug/ttally_controller.jpg)

It seams like the first underscore in the alt-text is interpreted. Therefor the next underscore in the path is interpreted too and the text between the two underscores is displayed italic.  

## The same picture with the underscore in the alt-text removed

![ttally controler class](img/lo_res/imagebug/ttally_controller.jpg)

Now it is displayed correct. It seems that the underscore in the alt-text is a problem. Maybe the underscore in the path contributes also to it.

I am not sure if an underscore in an alt-text should be interpreted as a text-formating character. If so please close this issue because its not a bug.

