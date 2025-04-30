const
  postcss = require('gulp-postcss'),
  gulp = require('gulp'),
  livereload = require('gulp-livereload'),
  rename = require('gulp-rename');

function css(cb) {
  gulp.src('styles.css')
    .pipe(postcss([
      require('tailwindcss'),
      require('autoprefixer'),
    ]))
    .pipe(rename('output.css'))
    .pipe(gulp.dest('css'))
    .pipe(livereload());
  cb();
};

exports.default = function() {
  livereload.listen();
  gulp.watch(['*.js', '/home/trey/dev/base.js'], css);
};
