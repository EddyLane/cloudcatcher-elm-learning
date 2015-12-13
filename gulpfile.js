const gulp = require('gulp');
const elm = require('gulp-elm');
const gutil = require('gulp-util');
const livereload = require('gulp-livereload');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
    return gulp.src('src/*.elm')
        .pipe(elm())
        .on('error', gutil.log)		
        .pipe(gulp.dest('dist/'))
        .pipe(livereload())	
        ;

});

gulp.task('watch', ['elm'], function () {
	livereload.listen();	
    gulp.watch('./**/*.elm', ['elm']);
});