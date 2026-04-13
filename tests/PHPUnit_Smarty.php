<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 *
 */

use Smarty\Exception;
use Smarty\TemplateBase;
use Smarty\Template;

/**
 * Smarty Test Case Fixture
 */
class PHPUnit_Smarty extends PHPUnit\Framework\TestCase
{

    /**
     * Smarty object
     *
     * @var \Smarty\Smarty
     */
    public $smarty = null;

    /**
     * Flag for initialization at first test
     *
     * @var bool
     */
    public static $init = true;

    /**
     * Count test number
     *
     * @var bool
     */
    public static $testNumber = 0;

    /**
     * Configuration data from config.xml
     *
     * @var array
     */
    public static $config = null;

    /**
     * Saved current working directory
     *
     * @var null
     */
    public static $cwd = null;

    /**
     * Temp directory base for this test class (compile, cache, templates_tmp)
     *
     * @var string|null
     */
    private static $tempBase = null;

    /**
     * Unique token for the current test class's temp directory.
     * Generated once per class, reset in tearDownAfterClass().
     *
     * @var string|null
     */
    private static $tempId = null;

    /**
     * Return the temp directory base for the current test class.
     *
     * @return string
     * @throws \LogicException If the temp directory base has not been initialized yet.
     */
    public static function getTempBase(): string
    {
        if (self::$tempBase === null) {
            throw new \LogicException(
                'Temp directory base has not been initialized. Call setUpSmarty() before using temp-path helpers.'
            );
        }
        return self::$tempBase;
    }

    /**
     * PDO object for Mysql tests
     *
     * @var PDO
     */
    public static $pdo = null;

    public static $pluginsdir = null;

    /**
     * This method is called before the first test of this test class is run.
     *
     */
    public static function setUpBeforeClass(): void
    {
        error_reporting(E_ALL & ~E_DEPRECATED & ~E_USER_DEPRECATED);
        self::$init = true;
        self::$pluginsdir =self::getSmartyPluginsDir();
    }

    /**
     * This method is called after the last test of this test class is run.
     *
     */
    public static function tearDownAfterClass(): void
    {
        //self::$pdo = null;
        self::$testNumber = 0;

        // Remove the unique temp directory for this test class unless the caller
        // wants to inspect the artifacts (e.g. for debugging a failure).
        if (!getenv('KEEP_SMARTY_TEST_ARTIFACTS') && self::$tempBase !== null && is_dir(self::$tempBase)) {
            self::removeDir(self::$tempBase);
        }

        self::$tempId = null;
        self::$tempBase = null;
    }

    /**
     * Recursively remove a directory, silently ignoring any errors.
     *
     * @param string $dir
     */
    private static function removeDir(string $dir): void
    {
        $dir = rtrim($dir, DIRECTORY_SEPARATOR);
        $items = @scandir($dir);
        if ($items === false) {
            return;
        }
        foreach ($items as $item) {
            if ($item === '.' || $item === '..') {
                continue;
            }
            $path = $dir . DIRECTORY_SEPARATOR . $item;
            if (is_dir($path) && !is_link($path)) {
                self::removeDir($path);
            } else {
                @unlink($path);
            }
        }
        @rmdir($dir);
    }

    /**
     * Constructs a test case with the given name.
     *
     * @param string $name
     * @param array  $data
     * @param string $dataName
     */
    public function __construct($name = null, array $data = array(), $dataName = '')
    {
        date_default_timezone_set('Europe/Berlin');
        parent::__construct($name, $data, $dataName);
    }

    /**
     * Compute the temp directory base for a given test directory.
     *
     * Returns a path unique to this test class run under sys_get_temp_dir(),
     * so that concurrent or sequential runs of different test classes never
     * share compiled/cached output. The unique token is generated once per
     * class lifetime and reset in tearDownAfterClass().
     *
     * Example:
     *   /path/to/smarty/tests/UnitTests/TagTests/If
     *     → /tmp/smarty-tests/UnitTests/TagTests/If/<unique-id>/
     *
     * @param string $dir absolute test directory
     * @return string absolute temp base directory (with trailing separator)
     */
    private static function getTempDir($dir)
    {
        // Lazily generate a unique token for this test class.
        if (self::$tempId === null) {
            self::$tempId = uniqid('', true);
        }
        $testsRoot = realpath(__DIR__);
        $realDir = realpath($dir) ?: $dir;
        // compute relative path from tests/ root
        if (strpos($realDir, $testsRoot) === 0) {
            $relative = substr($realDir, strlen($testsRoot));
        } else {
            // fallback: use full path hash
            $relative = DIRECTORY_SEPARATOR . md5($realDir);
        }
        return rtrim(sys_get_temp_dir(), DIRECTORY_SEPARATOR)
            . DIRECTORY_SEPARATOR . 'smarty-tests'
            . $relative
            . DIRECTORY_SEPARATOR . self::$tempId
            . DIRECTORY_SEPARATOR;
    }

    /**
     * Setup Smarty instance called for each test
     *
     * @param null $dir working directory
     */
    public function setUpSmarty($dir)
    {
        // set up current working directory
        chdir($dir);
        self::$cwd = getcwd();
        // compute temp base for this test directory
        self::$tempBase = self::getTempDir($dir);
        // create missing folders for test
        if (self::$init) {
            if (!is_dir(self::$tempBase . 'templates_c')) {
                mkdir(self::$tempBase . 'templates_c', 0775, true);
            }
            if (!is_dir(self::$tempBase . 'cache')) {
                mkdir(self::$tempBase . 'cache', 0775, true);
            }
			if (!is_dir(self::$tempBase . 'templates_tmp')) {
				mkdir(self::$tempBase . 'templates_tmp', 0775, true);
			}
            self::$init = false;
        }
        clearstatcache();

        // instance Smarty class
        $this->smarty = new \Smarty\Smarty();
        $this->smarty->setCompileDir(self::getTempBase() . 'templates_c');
        $this->smarty->setCacheDir(self::getTempBase() . 'cache');
		$this->smarty->addTemplateDir(self::getTempBase() . 'templates_tmp');

        // Clean output dirs once at the start of each test class run
        if (self::$testNumber === 0) {
            $this->cleanDirs();
        }
        self::$testNumber++;

    }

    /**
     * Create Mysql PDO object
     *
     */
    final public function getConnection()
    {
        if (PHPUnit_Smarty::$pdo == null) {
            try {
                PHPUnit_Smarty::$pdo = new PDO(DB_DSN, DB_USER, DB_PASSWD);
            }
            catch (PDOException $e) {
                throw new Exception('Mysql Resource failed: ' . $e->getMessage());
            }
            $timezone = date_default_timezone_get();
            $j = PHPUnit_Smarty::$pdo->exec("SET time_zone = '{$timezone}';");


        }
    }

    /**
     * Create table for Mysql resource
     *
     */
    public function initMysqlResource()
    {
        $this->getConnection();
        PHPUnit_Smarty::$pdo->exec("DROP TABLE `templates`");
        PHPUnit_Smarty::$pdo->exec("CREATE TABLE IF NOT EXISTS `templates` (
 `name` varchar(100) NOT NULL,
 `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
 `source` text,
PRIMARY KEY (`name`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8");

    }

    /**
     * Create table for Mysql cache resource
     *
     */
    public function initMysqlCache()
    {
        $this->getConnection();
        PHPUnit_Smarty::$pdo->exec("DROP TABLE `output_cache`");
         PHPUnit_Smarty::$pdo->exec("CREATE TABLE IF NOT EXISTS `output_cache` (
 `name` varchar(256) NOT NULL,
 `id` char(40) NOT NULL,
 `cache_id` varchar(250) DEFAULT NULL,
 `compile_id` varchar(250) DEFAULT NULL,
`modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
 `content` mediumblob NOT NULL,
PRIMARY KEY (`id`),
KEY `cache_id` (`cache_id`),
KEY `compile_id` (`compile_id`),
KEY `modified` (`modified`),
KEY `name` (`name`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8");
//     PHPUnit_Smarty::$pdo->exec("CREATE TABLE IF NOT EXISTS `output_cache` (
//`id` char(40) NOT NULL,
//`name` varchar(250) NOT NULL,
//`cache_id` varchar(250) DEFAULT NULL,
//`compile_id` varchar(250) DEFAULT NULL,
//`modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
//`expire` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
//`content` mediumblob NOT NULL,
///PRIMARY KEY (`id`),
//KEY `name` (`name`),
//KEY `cache_id` (`cache_id`),
//KEY `compile_id` (`compile_id`),
//KEY `modified` (`modified`),
//KEY `expire` (`expire`)
//) ENGINE=InnoDB DEFAULT CHARSET=utf8");

    }

    /**
     * Delete files in templates_c and cache folders
     *
     */
    public function cleanDirs()
    {
        $this->cleanCompileDir();
        $this->cleanCacheDir();
        $templatesTmpDir = self::getTempBase() . 'templates_tmp';
        if (is_dir($templatesTmpDir)) {
            $this->cleanDir($templatesTmpDir);
        }
    }

    /**
     * Make temporary template file
     *
     */
    public function makeTemplateFile($name, $code)
    {
        file_put_contents(self::getTempBase() . 'templates_tmp' . '/' . $name, $code);
    }

	public function removeTemplateFile($name) {
		unlink(self::getTempBase() . 'templates_tmp' . '/' . $name);
	}

    /**
     * Delete files in templates_c folder
     *
     */
    public function cleanCompileDir()
    {
        $smarty = $this->getSmarty();
        if (isset($smarty)) {
            $dir = $smarty->getCompileDir();
            $this->cleanDir($dir);
        }
    }

    /**
     * Delete files in cache folder
     *
     */
    public function cleanCacheDir()
    {
        $smarty = $this->getSmarty();
        if (isset($smarty)) {
            $dir = $smarty->getCacheDir();
            $this->cleanDir($dir);
         }
    }

    /**
     * Delete files and sub folders
     *
     * @param string $dir
     */
    public function cleanDir($dir)
    {
        $di = new RecursiveDirectoryIterator($dir);
        $ri = new RecursiveIteratorIterator($di, RecursiveIteratorIterator::CHILD_FIRST);
        foreach ($ri as $file) {
            if (substr(basename($file->getPathname()), 0, 1) === '.' || substr((string)$file,-4) === '.txt') {
                continue;
            }
            // directory ?
            if ($file->isDir()) {
                // delete folder if empty
                @rmdir($file->getPathname());
            } else {
                unlink($file->getPathname());
            }
        }
    }

    /**
     * Get PDO object
     *
     * @return null|PDO
     */
    final public function getPDO()
    {
        return PHPUnit_Smarty::$pdo;
    }

    /**
     * Remove "\r" and replace "\t" with spaces
     *
     * @param string $in
     *
     * @return mixed
     */
    public function normalizeString($in)
    {
        if (is_string($in)) {
            return str_replace(array("\r", "\t"), array('', '    '), $in);
        } else {
            return $in;
        }
    }

    /**
     * Remove all spaces
     *
     * @param string $in
     *
     * @return mixed
     */
    public function strip($in)
    {
        if (is_string($in)) {
            return preg_replace('/\s/', '', $in);
        } else {
            return $in;
        }
    }

    /**
     * Return source path
     *
     * @param TemplateBase $tpl  template object
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     * @param null|string                  $dir  optional template folder
     *
     * @return string
     * @throws \Exception
     */
    public function buildSourcePath($tpl, $name = null, $type = null, $dir = null)
    {
        $name = isset($name) ? $name : $tpl->getSource()->name;
        $type = isset($type) ? $type : $tpl->getSource()->type;
        $dir = isset($dir) ? $dir : $this->smarty->getTemplateDir(0);
        switch ($type) {
            case 'file':
            case 'filetest':
            case 'php':
                return $this->normalizePath($dir . $name);
            case 'mysqltest':
            case 'mysql':
                return sha1($type . ':' . $name);
            case 'string':
                $this->smarty->getTemplateDir();
                return sha1($name . $this->smarty->_joined_template_dir);
            default:
                throw new Exception("Unhandled source resource type '{$type}'");
        }
    }

    /**
     * Build template uid
     *
     * @param TemplateBase $tpl  template object
     * @param null|string                  $value
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     *
     * @return string
     * @throws \Exception
     */
    public function buildUid($tpl, $value = null, $name = null, $type = null)
    {
        $type = isset($type) ? $type : $tpl->getSource()->type;
        $name = isset($name) ? $name : $tpl->getSource()->name;
        switch ($type) {
            case 'php':
            case 'file':
            case 'filetest':
                if ($tpl instanceof \Smarty\Smarty) {
                    return sha1($this->normalizePath($this->smarty->getTemplateDir(0) . $name) .
                                $this->smarty->_joined_template_dir);
                }
                return sha1($tpl->getSource()->uid . $this->smarty->_joined_template_dir);
            case 'mysqltest':
            case 'mysql':
                return sha1($type . ':' . $name);
            case 'string':
                $this->smarty->getTemplateDir();
                return sha1($name . $this->smarty->_joined_template_dir);
            default:
                throw new Exception("Unhandled source resource type '{$type}'");
        }
    }

    /**
     * Normalize path
     *  - remove /./ and /../
     *  - make it absolute
     *
     * @param string $path file path
     *
     * @return string
     */
    public function normalizePath($path, $ds =null ,$absolute = true)
    {
        $ds = isset($ds) ? $ds : DIRECTORY_SEPARATOR;
        $nds = $ds == '/' ? '\\' : '/';
        $getcwd = getcwd();
        // normalize $ds
        $path = str_replace($nds, $ds, $path);
        preg_match('#^([a-zA-Z][:])?([.]{1,2}[\/\\\]+)?([\\\])?([.]{0,2}[\/\\\]+)?([[:print:]]*)#', $path, $match);
        if ($match[1] === '') {
            if ($match[ 2 ] !== '' || $match[ 2 ] . $match[ 3 ] . $match[ 4 ] === '') {
                $path = $getcwd . $ds . $path;
            } else if (\Smarty\Smarty::$_IS_WINDOWS && $match[ 3 ] !== '') {
                $path = substr($getcwd, 0, 2) . $path;
            }
        }
        $path = preg_replace('#[\\\/]+([.][\\\/]+)*#', $ds, $path);
        while (strrpos($path, '.' . $ds) !== false) {
            $path =
                preg_replace('#([\\\/]([^\\\/]+[\\\/]){2}([.][.][\\\/]){2})|([\\\/][^\\\/]+[\\\/][.][.][\\\/])#', $ds,
                             $path);
        }
        $cwd  = preg_replace('#[\\\/]#', $ds, $getcwd);
        $path = str_ireplace($cwd,$getcwd, $path);
        if (!$absolute) {
            $path = preg_replace('#'.$getcwd.'#', '', $path);
        }
        return $path;
    }

    /**
     * Return base name
     *
     * @param \Smarty\Template|\Smarty\TemplateBase $tpl  template object
     * @param null|string                                             $name optional template name
     * @param null|string                                             $type optional template type
     *
     * @return null|string
     */
    public function getBasename(Template $tpl, $name = null, $type = null)
    {
        $name = isset($name) ? $name : $tpl->getSource()->name;
        $type = isset($type) ? $type : $tpl->getSource()->type;
        switch ($type) {
            case 'file':
            case 'filetest':
                if (($_pos = strpos($name, ']')) !== false) {
                    $name = substr($name, $_pos + 1);
                }
                return basename($name);
            case 'mysqltest':
            case 'mysql':
                return $name;
            case 'string':
                return '';
            default:
                return null;
        }
    }

    /**
     * Return compiled file path
     *
     * @param \Smarty\Template|\Smarty\TemplateBase $tpl        template object
     * @param bool                                                    $sub        use sub directory flag
     * @param bool                                                    $caching    caching flag
     * @param null|string                                             $compile_id optional compile id
     * @param null|string                                             $name       optional template name
     * @param null|string                                             $type       optional template type
     * @param null|string                                             $dir        optional template folder
     *
     * @return string
     * @throws \Exception
     */
    public function buildCompiledPath(Template $tpl, $sub = true, $caching = false, $compile_id = null,
                                               $name = null, $type = null, $dir = null)
    {
        $sep = DIRECTORY_SEPARATOR;
        $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
        $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
        $uid = $this->buildUid($tpl, $sp, $name, $type);
        $_flag = '';
        if ($tpl->getSource() && $tpl->getSource()->isConfig) {
            $_flag = '_' . ((int) $tpl->getSmarty()->config_read_hidden + (int) $tpl->getSmarty()->config_booleanize * 2 +
                            (int) $tpl->getSmarty()->config_overwrite * 4);
        } else {
            $_flag = '_' . ((int) $tpl->getSmarty()->merge_compiled_includes + (int) $tpl->getSmarty()->escape_html * 2);
        }
        $_filepath = $uid . $_flag;
        // if use_sub_dirs, break file into directories
        if ($sub) {
            $_filepath =
                substr($_filepath, 0, 2) . $sep . substr($_filepath, 2, 2) . $sep . substr($_filepath, 4, 2) . $sep .
                $_filepath;
        }
        $_compile_dir_sep = $sub ? $sep : '^';
        if (isset($_compile_id)) {
            $_filepath = $_compile_id . $_compile_dir_sep . $_filepath;
        }
        // caching token
        if ($caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        }
        $_compile_dir = $tpl->getSmarty()->getCompileDir();
        // set basename if not specified
        $_basename = $this->getBasename($tpl, $name, $type);
        if ($_basename === null) {
            $_basename = basename(preg_replace('![^\w\/]+!', '_', $name));
        }
        // separate (optional) basename by dot
        if ($_basename) {
            $_basename = '.' . $_basename;
        }

        return $_compile_dir . $_filepath . '.' . $type . $_basename . $_cache . '.php';
    }

    /**
     * Return cache file path
     *
     * @param TemplateBase $tpl        template object
     * @param bool                         $sub        use sub directory flag
     * @param null|string                  $cache_id   optional cache id
     * @param null|string                  $compile_id optional compile id
     * @param null|string                  $name       optional template name
     * @param null|string                  $type       optional template type
     * @param null|string                  $dir        optional template folder
     * @param null|string                  $cacheType  optional cache resource type
     *
     * @return string
     * @throws \Exception
     */
    public function buildCachedPath(TemplateBase $tpl, $sub = true, $cache_id = null, $compile_id = null, $name = null, $type = null,
                                    $dir = null, $cacheType = null)
    {
        $cacheType = $cacheType ?? $tpl->getSmarty()->getCachingType();
        switch ($cacheType) {
            case 'file':
            case 'filetest':
                $sep = DIRECTORY_SEPARATOR;
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_filepath = $uid;
                // if use_sub_dirs, break file into directories
                if ($sub) {
                    $_filepath =
                        substr($_filepath, 0, 2) . $sep . substr($_filepath, 2, 2) . $sep . substr($_filepath, 4, 2) .
                        $sep . $_filepath;
                }
                $_compile_dir_sep = $sub ? $sep : '^';
                if (isset($_cache_id)) {
                    $_cache_id = str_replace('|', $_compile_dir_sep, $_cache_id) . $_compile_dir_sep;
                } else {
                    $_cache_id = '';
                }
                if (isset($_compile_id)) {
                    $_compile_id = $_compile_id . $_compile_dir_sep;
                } else {
                    $_compile_id = '';
                }
	            return $tpl->getSmarty()->getCacheDir() . $_cache_id . $_compile_id . $_filepath . '.' . basename($sp) . '.php';
            case 'mysqltest':
            case 'pdo':
            case 'foobar':
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                return sha1($uid . $_cache_id . $_compile_id);
            case 'memcachetest':
            case 'apc':
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                return sha1($uid) . '#' . preg_replace('#[^\w\|]+#S', '_', $tpl->template_resource) . '#' . $_cache_id .
                       '#' . $_compile_id;

            default:
                throw new Exception("Unhandled cache resource type '{$cacheType}'");
        }
    }

    /**
     * prefilter to insert test number
     *
     * @param  string                   $source
     * @param \Smarty\Template $tpl
     *
     * @return string
     */
    public function prefilterTest($source, Template $tpl)
    {
        return str_replace('#test#', "test:{\$test nocache} compiled:{$tpl->getTemplateVars('test')} rendered:{\$test}",
                           $source);
    }

    /**
     *  Gat Smarty object
     * @return null|\Smarty
     */
    public function getSmarty(){
        return $this->smarty;
    }

    public static function getSmartyPluginsDir(){
        if (is_dir(__DIR__ . '/../smarty/src/plugins')) {
            return __DIR__ . '/../smarty/src/plugins';
        } else if(is_dir(__DIR__ . '/../libs/plugins')) {
            return __DIR__ . '/../libs/plugins';
        }
    }
}
