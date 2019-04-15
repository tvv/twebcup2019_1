const
  path = require('path'),
  gulp = require('gulp'),
  sass = require('gulp-sass'),
  cssmin = require('gulp-cssmin'),
  rename = require('gulp-rename'),
  autoprefixer = require('gulp-autoprefixer'),
  minify = require('gulp-minify'),
  rev = require('./gulp-rev-append'),
  merge = require('merge2'),
  plumber = require('gulp-plumber'),
  buffer = require('vinyl-buffer'),
  webpack = require('webpack-stream'),
  replace = require('gulp-replace');

const paths = {
  styles: {
    src: ['scss/**/*.scss', 'css/**/*.css'],
    dest: path.join(__dirname, '../static/css')
  },
  scripts: {
    src: ['elm/**/*.elm', 'js/**/*.js'],
    dest: path.join(__dirname, '../static/js')
  },
  html: {
    src: ['index.html']
  }
};

gulp.task('js:rev', () => start_rev(path.join(__dirname, 'index.html')));
gulp.task('css:rev', () => start_rev(path.join(__dirname, 'index.html')));

function start_rev(src) {
  return gulp.src(src)
    .pipe(rev({
      file_path: path.join(__dirname, '..')
    }))
    .pipe(gulp.dest(path.join(__dirname, '../static')));
}

gulp.task('css:compile', () => {
  return gulp.src('scss/*.scss')
    .pipe(plumber())
    .pipe(sass())
    .pipe(cssmin().on('error', function (err) {
      console.log(err);
    }))
    .pipe(autoprefixer({
      grid: true,
      flexbox: true,
      browsers: ['last 5 versions']
    }))
    .pipe(rename({suffix: '.min'}))
    .pipe(gulp.dest(paths.styles.dest))
});

gulp.task('js:compile', () => {
  return gulp.src('js/index.js')
    .pipe(plumber())
    .pipe(webpack( require('./elm.webpack.config') ))
    .pipe(minify({
      ext: {min: '.js'},
      noSource: true
    }))
    .pipe(gulp.dest(paths.scripts.dest));
});

gulp.task('styles', gulp.series('css:compile', 'css:rev'));
gulp.task('scripts', gulp.series('js:compile', 'js:rev'));

gulp.task('css:watch', () => gulp.watch(paths.styles.src, gulp.parallel('styles')));
gulp.task('js:watch', () => gulp.watch(paths.scripts.src, gulp.parallel('scripts')));
gulp.task('html:watch', () => gulp.watch(paths.html.src, gulp.parallel('js:rev', 'css:rev')));

gulp.task('build', gulp.series(gulp.parallel('styles', 'scripts')));
gulp.task('watch', gulp.series('build', gulp.parallel('css:watch', 'js:watch', 'html:watch')));
