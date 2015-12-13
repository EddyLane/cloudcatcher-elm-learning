const gulp = require('gulp');
const elm = require('gulp-elm');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
    return gulp.src('src/*.elm')
        .pipe(elm())
        .pipe(gulp.dest('dist/'));
});

gulp.task('watch', ['elm'], function () {
    gulp.watch('./**/*.elm', ['elm']);
});