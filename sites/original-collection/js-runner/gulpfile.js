const
  gulp = require('gulp')
  postcss = require('gulp-postcss')
  child = require('child_process')
  rename = require('gulp-rename')

function css(cb) {
  gulp.src('tmp/tailwind.css')
    .pipe(postcss([
      require('tailwindcss'),
      require('autoprefixer'),
    ]))
    .pipe(rename('index.css'))
    .pipe(gulp.dest('./tmp/'));
  cb();
};

exports.default = function() {
  gulp.watch([
    '*.js',
    'tmp/tailwind.*',
  ],
    { ignoreInitial: false }, css);
};


// function reload() {
//   child.spawn('xdotool', ['search', '--onlyvisible', '--class', 'google-chrome', 'windowFocus', 'key', 'F5'], { stdio: 'inherit' });
//   child.spawn('xdotool', ['search', '--onlyvisible', '--class', 'st', 'windowFocus'], { stdio: 'inherit' });
// };
