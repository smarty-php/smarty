<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 *
 */

/**
 * Smarty Test Case Fixture
 */
class PHPUnit_Smarty extends PHPUnit_Framework_TestCase
{
    /**
     * Smarty object
     *
     * @var SmartyBC
     */
    public $smartyBC = null;

    /**
     * SmartyBC object
     *
     * @var Smarty
     */
    public $smarty = null;

    /**
     * Flag if test is using the Smarty object
     *
     * @var bool
     */
    public $loadSmarty = true;

    /**
     * Flag if test is using the SmartyBC object
     *
     * @var bool
     */
    public $loadSmartyBC = false;

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
     * PDO object for Mysql tests
     *
     * @var PDO
     */
    public static $pdo = null;

    public static $pluginsdir = null;

    /**
     * Default blacklist
     *
     * @var array
     */
    protected $backupStaticAttributesBlacklist = array('PHPUnit_Smarty' => array('config', 'pdo', 'init',
                                                                                 'testNumver', 'pluginsdir'),);

    /**
     * This method is called before the first test of this test class is run.
     *
     */
    public static function setUpBeforeClass()
    {
        error_reporting(E_ALL & ~E_STRICT);
        self::$init = true;
        self::$pluginsdir =self::getSmartyPluginsDir();
    }

    /**
     * This method is called after the last test of this test class is run.
     *
     */
    public static function tearDownAfterClass()
    {
        //self::$pdo = null;
        self::$testNumber = 0;
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
        if (!defined('individualFolders')) {
            define('individualFolders', true);
        }
        parent::__construct($name, $data, $dataName);
      $this->backupStaticAttributesBlacklist[ get_class($this) ] = array('init', 'config', 'pdo', 'testNumber');
    }

    /**
     * Setup Smarty instance called for each test
     *
     * @param null $dir working directory
     */
    public function setUpSmarty($dir = null)
    {
        static $s_dir;
        // set up current working directory
        chdir($dir);
        self::$cwd = getcwd();
        // create missing folders for test
        if (self::$init) {
            if (!is_dir($dir . '/templates')) {
                mkdir($dir . '/templates');
            }
            if (!is_dir($dir . '/configs')) {
                mkdir($dir . '/configs');
            }
            if (individualFolders != 'true') {
                if (!isset($s_dir[ $dir ])) {
                    $this->cleanDir($dir . '/templates_c');
                    $this->cleanDir($dir . '/cache');
                    if (is_dir($dir . '/templates_tmp')) {
                        $this->cleanDir($dir . '/templates_tmp');
                    }
                    $s_dir[ $dir ] = true;
                }
                $dir = dirname(__FILE__);
            }
            if (!is_dir($dir . '/templates_c')) {
                mkdir($dir . '/templates_c');
            }
            chmod($dir . '/templates_c', 0775);
            if (!is_dir($dir . '/cache')) {
                mkdir($dir . '/cache');
                chmod($dir . '/cache', 0775);
            }
            self::$init = false;
        }
        clearstatcache();
        // instance Smarty class
        if ($this->loadSmarty) {
            $this->smarty = new Smarty;
            if (individualFolders != 'true') {
                $this->smarty->setCompileDir(dirname(__FILE__) . '/templates_c');
                $this->smarty->setCacheDir(dirname(__FILE__) . '/cache');
            }
        }
        // instance SmartyBC class
        if ($this->loadSmartyBC) {
            $this->smartyBC = new SmartyBC;
            if (individualFolders != 'true') {
                $this->smartyBC->setCompileDir(dirname(__FILE__) . '/templates_c');
                $this->smartyBC->setCacheDir(dirname(__FILE__) . '/cache');
            }
        }
        $smarty = $this->getSmartyObj();
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
                throw new SmartyException('Mysql Resource failed: ' . $e->getMessage());
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
        if (is_dir(self::$cwd . '/templates_tmp')) {
            $this->cleanDir(self::$cwd . '/templates_tmp');
        }
        $this->assertTrue(true);
   }

    /**
     * Make temporary template file
     *
     */
    public function makeTemplateFile($name, $code)
    {
        if (!is_dir(self::$cwd . '/templates_tmp')) {
            mkdir(self::$cwd . '/templates_tmp');
            chmod(self::$cwd . '/templates_tmp', 0775);
        }
        $fileName = self::$cwd . '/templates_tmp/' . "{$name}";
        file_put_contents($fileName, $code);
    }

    /**
     * Delete files in templates_c folder
     *
     */
    public function cleanCompileDir()
    {
        $smarty = $this->getSmartyObj();
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
        $smarty = $this->getSmartyObj();
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
                if (!$ri->isDot()) {
                    // delete folder if empty
                    @rmdir($file->getPathname());
                }
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
     * @param Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     * @param null|string                  $dir  optional template folder
     *
     * @return string
     * @throws \Exception
     */
    public function buildSourcePath($tpl, $name = null, $type = null, $dir = null)
    {
        $name = isset($name) ? $name : $tpl->source->name;
        $type = isset($type) ? $type : $tpl->source->type;
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
     * @param Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                  $value
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     *
     * @return string
     * @throws \Exception
     */
    public function buildUid($tpl, $value = null, $name = null, $type = null)
    {
        $type = isset($type) ? $type : $tpl->source->type;
        $name = isset($name) ? $name : $tpl->source->name;
        switch ($type) {
            case 'php':
            case 'file':
            case 'filetest':
                if ($tpl instanceof Smarty) {
                    return sha1($this->normalizePath($this->smarty->getTemplateDir(0) . $name) .
                                $this->smarty->_joined_template_dir);
                }
                return sha1($tpl->source->filepath . $this->smarty->_joined_template_dir);
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
            } else if (Smarty::$_IS_WINDOWS && $match[ 3 ] !== '') {
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
     * @param \Smarty_Internal_Template|\Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                                             $name optional template name
     * @param null|string                                             $type optional template type
     *
     * @return null|string
     */
    public function getBasename(Smarty_Internal_Template $tpl, $name = null, $type = null)
    {
        $name = isset($name) ? $name : $tpl->source->name;
        $type = isset($type) ? $type : $tpl->source->type;
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
     * @param \Smarty_Internal_Template|\Smarty_Internal_TemplateBase $tpl        template object
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
    public function buildCompiledPath(Smarty_Internal_Template $tpl, $sub = true, $caching = false, $compile_id = null,
                                      $name = null, $type = null, $dir = null)
    {
        $sep = DIRECTORY_SEPARATOR;
        $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
        $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
        $uid = $this->buildUid($tpl, $sp, $name, $type);
        $_flag = '';
        if (isset($tpl->source) && $tpl->source->isConfig) {
            $_flag = '_' . ((int) $tpl->smarty->config_read_hidden + (int) $tpl->smarty->config_booleanize * 2 +
                            (int) $tpl->smarty->config_overwrite * 4);
        } else {
            $_flag = '_' . ((int) $tpl->smarty->merge_compiled_includes + (int) $tpl->smarty->escape_html * 2);
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
        $_compile_dir = $tpl->smarty->getCompileDir();
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
     * @param Smarty_Internal_TemplateBase $tpl        template object
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
    public function buildCachedPath($tpl, $sub = true, $cache_id = null, $compile_id = null, $name = null, $type = null,
                                    $dir = null, $cacheType = null)
    {
        $cacheType = isset($cacheType) ? $cacheType : $tpl->smarty->caching_type;
        switch ($cacheType) {
            case 'file':
            case 'filetest':
                $sep = DIRECTORY_SEPARATOR;
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_filepath = sha1($uid . $this->smarty->_joined_template_dir);
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
                $smarty = isset($tpl->smarty) ? $tpl->smarty : $tpl;
                $_cache_dir = $smarty->getCacheDir();
                return $_cache_dir . $_cache_id . $_compile_id . $_filepath . '.' . basename($sp) . '.php';
            case 'mysql':
            case 'mysqltest':
            case 'pdo':
            case 'foobar':
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                return sha1($uid . $_cache_id . $_compile_id);
            case 'memcachetest':
            case 'acp':
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
     * @param \Smarty_Internal_Template $tpl
     *
     * @return string
     */
    public function prefilterTest($source, Smarty_Internal_Template $tpl)
    {
        return str_replace('#test#', "test:{\$test nocache} compiled:{$tpl->getTemplateVars('test')} rendered:{\$test}",
                           $source);
    }

    /**
     *  Gat Smarty object
     * @return null|\Smarty|\SmartyBC
     */
    public function getSmartyObj(){
        return isset($this->smarty) ? $this->smarty : (isset($this->smartyBC) ? $this->smartyBC : null);
    }

    public static function getSmartyPluginsDir(){
        if (is_dir(dirname(__FILE__) . '/../smarty/libs/plugins')) {
            return dirname(__FILE__) . '/../smarty/libs/plugins';
        } else if(is_dir(dirname(__FILE__) . '/../libs/plugins')) {
            return dirname(__FILE__) . '/../libs/plugins';
        }
    }
    /**
     * Tears down the fixture
     * This method is called after a test is executed.
     *
     */
    protected function tearDown()
    {
        if (class_exists('Smarty_Internal_TemplateCompilerBase') &&
            isset(Smarty_Internal_TemplateCompilerBase::$_tag_objects)
        ) {
            Smarty_Internal_TemplateCompilerBase::$_tag_objects = array();
        }
        if (isset($this->smarty->smarty)) {
            $this->smarty->smarty = null;
        }
        if (isset($this->smarty)) {
            $this->smarty = null;
        }
        if (isset($this->smartyBC->smarty)) {
            $this->smartyBC->smarty = null;
        }
        if (isset($this->smartyBC)) {
            $this->smartyBC = null;
        }
    }
}
