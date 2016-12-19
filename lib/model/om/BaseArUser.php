<?php

/**
 * Base class that represents a row from the 'ar_user' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArUser extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArUserPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_party_id field.
	 * @var        int
	 */
	protected $ar_party_id;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the login field.
	 * @var        string
	 */
	protected $login;

	/**
	 * The value for the password field.
	 * @var        string
	 */
	protected $password;

	/**
	 * The value for the is_enabled field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $is_enabled;

	/**
	 * The value for the is_root_admin field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_root_admin;

	/**
	 * @var        ArParty
	 */
	protected $aArParty;

	/**
	 * @var        ArOrganizationUnit
	 */
	protected $aArOrganizationUnit;

	/**
	 * @var        array ArUserChangePasswordRequest[] Collection to store aggregation of ArUserChangePasswordRequest objects.
	 */
	protected $collArUserChangePasswordRequests;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUserChangePasswordRequests.
	 */
	private $lastArUserChangePasswordRequestCriteria = null;

	/**
	 * @var        array ArUserHasRole[] Collection to store aggregation of ArUserHasRole objects.
	 */
	protected $collArUserHasRoles;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUserHasRoles.
	 */
	private $lastArUserHasRoleCriteria = null;

	/**
	 * @var        array ArUserHasPermission[] Collection to store aggregation of ArUserHasPermission objects.
	 */
	protected $collArUserHasPermissions;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUserHasPermissions.
	 */
	private $lastArUserHasPermissionCriteria = null;

	/**
	 * @var        array ArReport[] Collection to store aggregation of ArReport objects.
	 */
	protected $collArReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReports.
	 */
	private $lastArReportCriteria = null;

	/**
	 * @var        array ArReportToRead[] Collection to store aggregation of ArReportToRead objects.
	 */
	protected $collArReportToReads;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportToReads.
	 */
	private $lastArReportToReadCriteria = null;

	/**
	 * @var        array ArReportToReadUserView[] Collection to store aggregation of ArReportToReadUserView objects.
	 */
	protected $collArReportToReadUserViews;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportToReadUserViews.
	 */
	private $lastArReportToReadUserViewCriteria = null;

	/**
	 * @var        array ArUserCanViewReport[] Collection to store aggregation of ArUserCanViewReport objects.
	 */
	protected $collArUserCanViewReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUserCanViewReports.
	 */
	private $lastArUserCanViewReportCriteria = null;

	/**
	 * Flag to prevent endless save loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInSave = false;

	/**
	 * Flag to prevent endless validation loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInValidation = false;

	// symfony behavior
	
	const PEER = 'ArUserPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_enabled = true;
		$this->is_root_admin = false;
	}

	/**
	 * Initializes internal state of BaseArUser object.
	 * @see        applyDefaults()
	 */
	public function __construct()
	{
		parent::__construct();
		$this->applyDefaultValues();
	}

	/**
	 * Get the [id] column value.
	 * 
	 * @return     int
	 */
	public function getId()
	{
		return $this->id;
	}

	/**
	 * Get the [ar_party_id] column value.
	 * 
	 * @return     int
	 */
	public function getArPartyId()
	{
		return $this->ar_party_id;
	}

	/**
	 * Get the [ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getArOrganizationUnitId()
	{
		return $this->ar_organization_unit_id;
	}

	/**
	 * Get the [login] column value.
	 * 
	 * @return     string
	 */
	public function getLogin()
	{
		return $this->login;
	}

	/**
	 * Get the [password] column value.
	 * 
	 * @return     string
	 */
	public function getPassword()
	{
		return $this->password;
	}

	/**
	 * Get the [is_enabled] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsEnabled()
	{
		return $this->is_enabled;
	}

	/**
	 * Get the [is_root_admin] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsRootAdmin()
	{
		return $this->is_root_admin;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArUserPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_party_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setArPartyId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_party_id !== $v) {
			$this->ar_party_id = $v;
			$this->modifiedColumns[] = ArUserPeer::AR_PARTY_ID;
		}

		if ($this->aArParty !== null && $this->aArParty->getId() !== $v) {
			$this->aArParty = null;
		}

		return $this;
	} // setArPartyId()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArUserPeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [login] column.
	 * 
	 * @param      string $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setLogin($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->login !== $v) {
			$this->login = $v;
			$this->modifiedColumns[] = ArUserPeer::LOGIN;
		}

		return $this;
	} // setLogin()

	/**
	 * Set the value of [password] column.
	 * 
	 * @param      string $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setPassword($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->password !== $v) {
			$this->password = $v;
			$this->modifiedColumns[] = ArUserPeer::PASSWORD;
		}

		return $this;
	} // setPassword()

	/**
	 * Set the value of [is_enabled] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setIsEnabled($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_enabled !== $v || $this->isNew()) {
			$this->is_enabled = $v;
			$this->modifiedColumns[] = ArUserPeer::IS_ENABLED;
		}

		return $this;
	} // setIsEnabled()

	/**
	 * Set the value of [is_root_admin] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArUser The current object (for fluent API support)
	 */
	public function setIsRootAdmin($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_root_admin !== $v || $this->isNew()) {
			$this->is_root_admin = $v;
			$this->modifiedColumns[] = ArUserPeer::IS_ROOT_ADMIN;
		}

		return $this;
	} // setIsRootAdmin()

	/**
	 * Indicates whether the columns in this object are only set to default values.
	 *
	 * This method can be used in conjunction with isModified() to indicate whether an object is both
	 * modified _and_ has some values set which are non-default.
	 *
	 * @return     boolean Whether the columns in this object are only been set with default values.
	 */
	public function hasOnlyDefaultValues()
	{
			if ($this->is_enabled !== true) {
				return false;
			}

			if ($this->is_root_admin !== false) {
				return false;
			}

		// otherwise, everything was equal, so return TRUE
		return true;
	} // hasOnlyDefaultValues()

	/**
	 * Hydrates (populates) the object variables with values from the database resultset.
	 *
	 * An offset (0-based "start column") is specified so that objects can be hydrated
	 * with a subset of the columns in the resultset rows.  This is needed, for example,
	 * for results of JOIN queries where the resultset row includes columns from two or
	 * more tables.
	 *
	 * @param      array $row The row returned by PDOStatement->fetch(PDO::FETCH_NUM)
	 * @param      int $startcol 0-based offset column which indicates which restultset column to start with.
	 * @param      boolean $rehydrate Whether this object is being re-hydrated from the database.
	 * @return     int next starting column
	 * @throws     PropelException  - Any caught Exception will be rewrapped as a PropelException.
	 */
	public function hydrate($row, $startcol = 0, $rehydrate = false)
	{
		try {

			$this->id = ($row[$startcol + 0] !== null) ? (int) $row[$startcol + 0] : null;
			$this->ar_party_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->login = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->password = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->is_enabled = ($row[$startcol + 5] !== null) ? (boolean) $row[$startcol + 5] : null;
			$this->is_root_admin = ($row[$startcol + 6] !== null) ? (boolean) $row[$startcol + 6] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 7; // 7 = ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArUser object", $e);
		}
	}

	/**
	 * Checks and repairs the internal consistency of the object.
	 *
	 * This method is executed after an already-instantiated object is re-hydrated
	 * from the database.  It exists to check any foreign keys to make sure that
	 * the objects related to the current object are correct based on foreign key.
	 *
	 * You can override this method in the stub class, but you should always invoke
	 * the base method from the overridden method (i.e. parent::ensureConsistency()),
	 * in case your model changes.
	 *
	 * @throws     PropelException
	 */
	public function ensureConsistency()
	{

		if ($this->aArParty !== null && $this->ar_party_id !== $this->aArParty->getId()) {
			$this->aArParty = null;
		}
		if ($this->aArOrganizationUnit !== null && $this->ar_organization_unit_id !== $this->aArOrganizationUnit->getId()) {
			$this->aArOrganizationUnit = null;
		}
	} // ensureConsistency

	/**
	 * Reloads this object from datastore based on primary key and (optionally) resets all associated objects.
	 *
	 * This will only work if the object has been saved and has a valid primary key set.
	 *
	 * @param      boolean $deep (optional) Whether to also de-associated any related objects.
	 * @param      PropelPDO $con (optional) The PropelPDO connection to use.
	 * @return     void
	 * @throws     PropelException - if this object is deleted, unsaved or doesn't have pk match in db
	 */
	public function reload($deep = false, PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("Cannot reload a deleted object.");
		}

		if ($this->isNew()) {
			throw new PropelException("Cannot reload an unsaved object.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArUserPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArUserPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArParty = null;
			$this->aArOrganizationUnit = null;
			$this->collArUserChangePasswordRequests = null;
			$this->lastArUserChangePasswordRequestCriteria = null;

			$this->collArUserHasRoles = null;
			$this->lastArUserHasRoleCriteria = null;

			$this->collArUserHasPermissions = null;
			$this->lastArUserHasPermissionCriteria = null;

			$this->collArReports = null;
			$this->lastArReportCriteria = null;

			$this->collArReportToReads = null;
			$this->lastArReportToReadCriteria = null;

			$this->collArReportToReadUserViews = null;
			$this->lastArReportToReadUserViewCriteria = null;

			$this->collArUserCanViewReports = null;
			$this->lastArUserCanViewReportCriteria = null;

		} // if (deep)
	}

	/**
	 * Removes this object from datastore and sets delete attribute.
	 *
	 * @param      PropelPDO $con
	 * @return     void
	 * @throws     PropelException
	 * @see        BaseObject::setDeleted()
	 * @see        BaseObject::isDeleted()
	 */
	public function delete(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArUserPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArUserPeer::doDelete($this, $con);
				$this->postDelete($con);
				$this->setDeleted(true);
				$con->commit();
			} else {
				$con->commit();
			}
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Persists this object to the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All modified related objects will also be persisted in the doSave()
	 * method.  This method wraps all precipitate database operations in a
	 * single transaction.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        doSave()
	 */
	public function save(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("You cannot save an object that has been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArUserPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		$isInsert = $this->isNew();
		try {
			$ret = $this->preSave($con);
			if ($isInsert) {
				$ret = $ret && $this->preInsert($con);
			} else {
				$ret = $ret && $this->preUpdate($con);
			}
			if ($ret) {
				$affectedRows = $this->doSave($con);
				if ($isInsert) {
					$this->postInsert($con);
				} else {
					$this->postUpdate($con);
				}
				$this->postSave($con);
				ArUserPeer::addInstanceToPool($this);
			} else {
				$affectedRows = 0;
			}
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Performs the work of inserting or updating the row in the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All related objects are also updated in this method.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        save()
	 */
	protected function doSave(PropelPDO $con)
	{
		$affectedRows = 0; // initialize var to track total num of affected rows
		if (!$this->alreadyInSave) {
			$this->alreadyInSave = true;

			// We call the save method on the following object(s) if they
			// were passed to this object by their coresponding set
			// method.  This object relates to these object(s) by a
			// foreign key reference.

			if ($this->aArParty !== null) {
				if ($this->aArParty->isModified() || $this->aArParty->isNew()) {
					$affectedRows += $this->aArParty->save($con);
				}
				$this->setArParty($this->aArParty);
			}

			if ($this->aArOrganizationUnit !== null) {
				if ($this->aArOrganizationUnit->isModified() || $this->aArOrganizationUnit->isNew()) {
					$affectedRows += $this->aArOrganizationUnit->save($con);
				}
				$this->setArOrganizationUnit($this->aArOrganizationUnit);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArUserPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArUserPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArUserPeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArUserChangePasswordRequests !== null) {
				foreach ($this->collArUserChangePasswordRequests as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUserHasRoles !== null) {
				foreach ($this->collArUserHasRoles as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUserHasPermissions !== null) {
				foreach ($this->collArUserHasPermissions as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReports !== null) {
				foreach ($this->collArReports as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportToReads !== null) {
				foreach ($this->collArReportToReads as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportToReadUserViews !== null) {
				foreach ($this->collArReportToReadUserViews as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUserCanViewReports !== null) {
				foreach ($this->collArUserCanViewReports as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			$this->alreadyInSave = false;

		}
		return $affectedRows;
	} // doSave()

	/**
	 * Array of ValidationFailed objects.
	 * @var        array ValidationFailed[]
	 */
	protected $validationFailures = array();

	/**
	 * Gets any ValidationFailed objects that resulted from last call to validate().
	 *
	 *
	 * @return     array ValidationFailed[]
	 * @see        validate()
	 */
	public function getValidationFailures()
	{
		return $this->validationFailures;
	}

	/**
	 * Validates the objects modified field values and all objects related to this table.
	 *
	 * If $columns is either a column name or an array of column names
	 * only those columns are validated.
	 *
	 * @param      mixed $columns Column name or an array of column names.
	 * @return     boolean Whether all columns pass validation.
	 * @see        doValidate()
	 * @see        getValidationFailures()
	 */
	public function validate($columns = null)
	{
		$res = $this->doValidate($columns);
		if ($res === true) {
			$this->validationFailures = array();
			return true;
		} else {
			$this->validationFailures = $res;
			return false;
		}
	}

	/**
	 * This function performs the validation work for complex object models.
	 *
	 * In addition to checking the current object, all related objects will
	 * also be validated.  If all pass then <code>true</code> is returned; otherwise
	 * an aggreagated array of ValidationFailed objects will be returned.
	 *
	 * @param      array $columns Array of column names to validate.
	 * @return     mixed <code>true</code> if all validations pass; array of <code>ValidationFailed</code> objets otherwise.
	 */
	protected function doValidate($columns = null)
	{
		if (!$this->alreadyInValidation) {
			$this->alreadyInValidation = true;
			$retval = null;

			$failureMap = array();


			// We call the validate method on the following object(s) if they
			// were passed to this object by their coresponding set
			// method.  This object relates to these object(s) by a
			// foreign key reference.

			if ($this->aArParty !== null) {
				if (!$this->aArParty->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArParty->getValidationFailures());
				}
			}

			if ($this->aArOrganizationUnit !== null) {
				if (!$this->aArOrganizationUnit->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArOrganizationUnit->getValidationFailures());
				}
			}


			if (($retval = ArUserPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArUserChangePasswordRequests !== null) {
					foreach ($this->collArUserChangePasswordRequests as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUserHasRoles !== null) {
					foreach ($this->collArUserHasRoles as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUserHasPermissions !== null) {
					foreach ($this->collArUserHasPermissions as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReports !== null) {
					foreach ($this->collArReports as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportToReads !== null) {
					foreach ($this->collArReportToReads as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportToReadUserViews !== null) {
					foreach ($this->collArReportToReadUserViews as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUserCanViewReports !== null) {
					foreach ($this->collArUserCanViewReports as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}


			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	/**
	 * Retrieves a field from the object by name passed in as a string.
	 *
	 * @param      string $name name
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     mixed Value of field.
	 */
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArUserPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		$field = $this->getByPosition($pos);
		return $field;
	}

	/**
	 * Retrieves a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @return     mixed Value of field at $pos
	 */
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getId();
				break;
			case 1:
				return $this->getArPartyId();
				break;
			case 2:
				return $this->getArOrganizationUnitId();
				break;
			case 3:
				return $this->getLogin();
				break;
			case 4:
				return $this->getPassword();
				break;
			case 5:
				return $this->getIsEnabled();
				break;
			case 6:
				return $this->getIsRootAdmin();
				break;
			default:
				return null;
				break;
		} // switch()
	}

	/**
	 * Exports the object as an array.
	 *
	 * You can specify the key type of the array by passing one of the class
	 * type constants.
	 *
	 * @param      string $keyType (optional) One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                        BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. Defaults to BasePeer::TYPE_PHPNAME.
	 * @param      boolean $includeLazyLoadColumns (optional) Whether to include lazy loaded columns.  Defaults to TRUE.
	 * @return     an associative array containing the field names (as keys) and field values
	 */
	public function toArray($keyType = BasePeer::TYPE_PHPNAME, $includeLazyLoadColumns = true)
	{
		$keys = ArUserPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArPartyId(),
			$keys[2] => $this->getArOrganizationUnitId(),
			$keys[3] => $this->getLogin(),
			$keys[4] => $this->getPassword(),
			$keys[5] => $this->getIsEnabled(),
			$keys[6] => $this->getIsRootAdmin(),
		);
		return $result;
	}

	/**
	 * Sets a field from the object by name passed in as a string.
	 *
	 * @param      string $name peer name
	 * @param      mixed $value field value
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     void
	 */
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArUserPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	/**
	 * Sets a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @param      mixed $value field value
	 * @return     void
	 */
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setId($value);
				break;
			case 1:
				$this->setArPartyId($value);
				break;
			case 2:
				$this->setArOrganizationUnitId($value);
				break;
			case 3:
				$this->setLogin($value);
				break;
			case 4:
				$this->setPassword($value);
				break;
			case 5:
				$this->setIsEnabled($value);
				break;
			case 6:
				$this->setIsRootAdmin($value);
				break;
		} // switch()
	}

	/**
	 * Populates the object using an array.
	 *
	 * This is particularly useful when populating an object from one of the
	 * request arrays (e.g. $_POST).  This method goes through the column
	 * names, checking to see whether a matching key exists in populated
	 * array. If so the setByName() method is called for that column.
	 *
	 * You can specify the key type of the array by additionally passing one
	 * of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME,
	 * BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM.
	 * The default key type is the column's phpname (e.g. 'AuthorId')
	 *
	 * @param      array  $arr     An array to populate the object from.
	 * @param      string $keyType The type of keys the array uses.
	 * @return     void
	 */
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArUserPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArPartyId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArOrganizationUnitId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setLogin($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setPassword($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setIsEnabled($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setIsRootAdmin($arr[$keys[6]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArUserPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArUserPeer::ID)) $criteria->add(ArUserPeer::ID, $this->id);
		if ($this->isColumnModified(ArUserPeer::AR_PARTY_ID)) $criteria->add(ArUserPeer::AR_PARTY_ID, $this->ar_party_id);
		if ($this->isColumnModified(ArUserPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArUserPeer::LOGIN)) $criteria->add(ArUserPeer::LOGIN, $this->login);
		if ($this->isColumnModified(ArUserPeer::PASSWORD)) $criteria->add(ArUserPeer::PASSWORD, $this->password);
		if ($this->isColumnModified(ArUserPeer::IS_ENABLED)) $criteria->add(ArUserPeer::IS_ENABLED, $this->is_enabled);
		if ($this->isColumnModified(ArUserPeer::IS_ROOT_ADMIN)) $criteria->add(ArUserPeer::IS_ROOT_ADMIN, $this->is_root_admin);

		return $criteria;
	}

	/**
	 * Builds a Criteria object containing the primary key for this object.
	 *
	 * Unlike buildCriteria() this method includes the primary key values regardless
	 * of whether or not they have been modified.
	 *
	 * @return     Criteria The Criteria object containing value(s) for primary key(s).
	 */
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArUserPeer::DATABASE_NAME);

		$criteria->add(ArUserPeer::ID, $this->id);

		return $criteria;
	}

	/**
	 * Returns the primary key for this object (row).
	 * @return     int
	 */
	public function getPrimaryKey()
	{
		return $this->getId();
	}

	/**
	 * Generic method to set the primary key (id column).
	 *
	 * @param      int $key Primary key.
	 * @return     void
	 */
	public function setPrimaryKey($key)
	{
		$this->setId($key);
	}

	/**
	 * Sets contents of passed object to values from current object.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      object $copyObj An object of ArUser (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArPartyId($this->ar_party_id);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setLogin($this->login);

		$copyObj->setPassword($this->password);

		$copyObj->setIsEnabled($this->is_enabled);

		$copyObj->setIsRootAdmin($this->is_root_admin);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArUserChangePasswordRequests() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUserChangePasswordRequest($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUserHasRoles() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUserHasRole($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUserHasPermissions() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUserHasPermission($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReport($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportToReads() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportToRead($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportToReadUserViews() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportToReadUserView($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUserCanViewReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUserCanViewReport($relObj->copy($deepCopy));
				}
			}

		} // if ($deepCopy)


		$copyObj->setNew(true);

		$copyObj->setId(NULL); // this is a auto-increment column, so set to default value

	}

	/**
	 * Makes a copy of this object that will be inserted as a new row in table when saved.
	 * It creates a new object filling in the simple attributes, but skipping any primary
	 * keys that are defined for the table.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @return     ArUser Clone of current object.
	 * @throws     PropelException
	 */
	public function copy($deepCopy = false)
	{
		// we use get_class(), because this might be a subclass
		$clazz = get_class($this);
		$copyObj = new $clazz();
		$this->copyInto($copyObj, $deepCopy);
		return $copyObj;
	}

	/**
	 * Returns a peer instance associated with this om.
	 *
	 * Since Peer classes are not to have any instance attributes, this method returns the
	 * same instance for all member of this class. The method could therefore
	 * be static, but this would prevent one from overriding the behavior.
	 *
	 * @return     ArUserPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArUserPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArParty object.
	 *
	 * @param      ArParty $v
	 * @return     ArUser The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArParty(ArParty $v = null)
	{
		if ($v === null) {
			$this->setArPartyId(NULL);
		} else {
			$this->setArPartyId($v->getId());
		}

		$this->aArParty = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArParty object, it will not be re-added.
		if ($v !== null) {
			$v->addArUser($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArParty object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArParty The associated ArParty object.
	 * @throws     PropelException
	 */
	public function getArParty(PropelPDO $con = null)
	{
		if ($this->aArParty === null && ($this->ar_party_id !== null)) {
			$this->aArParty = ArPartyPeer::retrieveByPk($this->ar_party_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArParty->addArUsers($this);
			 */
		}
		return $this->aArParty;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArUser The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArOrganizationUnit(ArOrganizationUnit $v = null)
	{
		if ($v === null) {
			$this->setArOrganizationUnitId(NULL);
		} else {
			$this->setArOrganizationUnitId($v->getId());
		}

		$this->aArOrganizationUnit = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArOrganizationUnit object, it will not be re-added.
		if ($v !== null) {
			$v->addArUser($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArOrganizationUnit object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArOrganizationUnit The associated ArOrganizationUnit object.
	 * @throws     PropelException
	 */
	public function getArOrganizationUnit(PropelPDO $con = null)
	{
		if ($this->aArOrganizationUnit === null && ($this->ar_organization_unit_id !== null)) {
			$this->aArOrganizationUnit = ArOrganizationUnitPeer::retrieveByPk($this->ar_organization_unit_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArOrganizationUnit->addArUsers($this);
			 */
		}
		return $this->aArOrganizationUnit;
	}

	/**
	 * Clears out the collArUserChangePasswordRequests collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUserChangePasswordRequests()
	 */
	public function clearArUserChangePasswordRequests()
	{
		$this->collArUserChangePasswordRequests = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUserChangePasswordRequests collection (array).
	 *
	 * By default this just sets the collArUserChangePasswordRequests collection to an empty array (like clearcollArUserChangePasswordRequests());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUserChangePasswordRequests()
	{
		$this->collArUserChangePasswordRequests = array();
	}

	/**
	 * Gets an array of ArUserChangePasswordRequest objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArUserChangePasswordRequests from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUserChangePasswordRequest[]
	 * @throws     PropelException
	 */
	public function getArUserChangePasswordRequests($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserChangePasswordRequests === null) {
			if ($this->isNew()) {
			   $this->collArUserChangePasswordRequests = array();
			} else {

				$criteria->add(ArUserChangePasswordRequestPeer::AR_USER_ID, $this->id);

				ArUserChangePasswordRequestPeer::addSelectColumns($criteria);
				$this->collArUserChangePasswordRequests = ArUserChangePasswordRequestPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserChangePasswordRequestPeer::AR_USER_ID, $this->id);

				ArUserChangePasswordRequestPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserChangePasswordRequestCriteria) || !$this->lastArUserChangePasswordRequestCriteria->equals($criteria)) {
					$this->collArUserChangePasswordRequests = ArUserChangePasswordRequestPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserChangePasswordRequestCriteria = $criteria;
		return $this->collArUserChangePasswordRequests;
	}

	/**
	 * Returns the number of related ArUserChangePasswordRequest objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUserChangePasswordRequest objects.
	 * @throws     PropelException
	 */
	public function countArUserChangePasswordRequests(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUserChangePasswordRequests === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserChangePasswordRequestPeer::AR_USER_ID, $this->id);

				$count = ArUserChangePasswordRequestPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserChangePasswordRequestPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArUserChangePasswordRequestCriteria) || !$this->lastArUserChangePasswordRequestCriteria->equals($criteria)) {
					$count = ArUserChangePasswordRequestPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUserChangePasswordRequests);
				}
			} else {
				$count = count($this->collArUserChangePasswordRequests);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUserChangePasswordRequest object to this object
	 * through the ArUserChangePasswordRequest foreign key attribute.
	 *
	 * @param      ArUserChangePasswordRequest $l ArUserChangePasswordRequest
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUserChangePasswordRequest(ArUserChangePasswordRequest $l)
	{
		if ($this->collArUserChangePasswordRequests === null) {
			$this->initArUserChangePasswordRequests();
		}
		if (!in_array($l, $this->collArUserChangePasswordRequests, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUserChangePasswordRequests, $l);
			$l->setArUser($this);
		}
	}

	/**
	 * Clears out the collArUserHasRoles collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUserHasRoles()
	 */
	public function clearArUserHasRoles()
	{
		$this->collArUserHasRoles = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUserHasRoles collection (array).
	 *
	 * By default this just sets the collArUserHasRoles collection to an empty array (like clearcollArUserHasRoles());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUserHasRoles()
	{
		$this->collArUserHasRoles = array();
	}

	/**
	 * Gets an array of ArUserHasRole objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArUserHasRoles from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUserHasRole[]
	 * @throws     PropelException
	 */
	public function getArUserHasRoles($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserHasRoles === null) {
			if ($this->isNew()) {
			   $this->collArUserHasRoles = array();
			} else {

				$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

				ArUserHasRolePeer::addSelectColumns($criteria);
				$this->collArUserHasRoles = ArUserHasRolePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

				ArUserHasRolePeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserHasRoleCriteria) || !$this->lastArUserHasRoleCriteria->equals($criteria)) {
					$this->collArUserHasRoles = ArUserHasRolePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserHasRoleCriteria = $criteria;
		return $this->collArUserHasRoles;
	}

	/**
	 * Returns the number of related ArUserHasRole objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUserHasRole objects.
	 * @throws     PropelException
	 */
	public function countArUserHasRoles(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUserHasRoles === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

				$count = ArUserHasRolePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArUserHasRoleCriteria) || !$this->lastArUserHasRoleCriteria->equals($criteria)) {
					$count = ArUserHasRolePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUserHasRoles);
				}
			} else {
				$count = count($this->collArUserHasRoles);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUserHasRole object to this object
	 * through the ArUserHasRole foreign key attribute.
	 *
	 * @param      ArUserHasRole $l ArUserHasRole
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUserHasRole(ArUserHasRole $l)
	{
		if ($this->collArUserHasRoles === null) {
			$this->initArUserHasRoles();
		}
		if (!in_array($l, $this->collArUserHasRoles, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUserHasRoles, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArUserHasRoles from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArUserHasRolesJoinArRole($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserHasRoles === null) {
			if ($this->isNew()) {
				$this->collArUserHasRoles = array();
			} else {

				$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

				$this->collArUserHasRoles = ArUserHasRolePeer::doSelectJoinArRole($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserHasRolePeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArUserHasRoleCriteria) || !$this->lastArUserHasRoleCriteria->equals($criteria)) {
				$this->collArUserHasRoles = ArUserHasRolePeer::doSelectJoinArRole($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserHasRoleCriteria = $criteria;

		return $this->collArUserHasRoles;
	}

	/**
	 * Clears out the collArUserHasPermissions collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUserHasPermissions()
	 */
	public function clearArUserHasPermissions()
	{
		$this->collArUserHasPermissions = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUserHasPermissions collection (array).
	 *
	 * By default this just sets the collArUserHasPermissions collection to an empty array (like clearcollArUserHasPermissions());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUserHasPermissions()
	{
		$this->collArUserHasPermissions = array();
	}

	/**
	 * Gets an array of ArUserHasPermission objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArUserHasPermissions from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUserHasPermission[]
	 * @throws     PropelException
	 */
	public function getArUserHasPermissions($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserHasPermissions === null) {
			if ($this->isNew()) {
			   $this->collArUserHasPermissions = array();
			} else {

				$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

				ArUserHasPermissionPeer::addSelectColumns($criteria);
				$this->collArUserHasPermissions = ArUserHasPermissionPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

				ArUserHasPermissionPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserHasPermissionCriteria) || !$this->lastArUserHasPermissionCriteria->equals($criteria)) {
					$this->collArUserHasPermissions = ArUserHasPermissionPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserHasPermissionCriteria = $criteria;
		return $this->collArUserHasPermissions;
	}

	/**
	 * Returns the number of related ArUserHasPermission objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUserHasPermission objects.
	 * @throws     PropelException
	 */
	public function countArUserHasPermissions(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUserHasPermissions === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

				$count = ArUserHasPermissionPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArUserHasPermissionCriteria) || !$this->lastArUserHasPermissionCriteria->equals($criteria)) {
					$count = ArUserHasPermissionPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUserHasPermissions);
				}
			} else {
				$count = count($this->collArUserHasPermissions);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUserHasPermission object to this object
	 * through the ArUserHasPermission foreign key attribute.
	 *
	 * @param      ArUserHasPermission $l ArUserHasPermission
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUserHasPermission(ArUserHasPermission $l)
	{
		if ($this->collArUserHasPermissions === null) {
			$this->initArUserHasPermissions();
		}
		if (!in_array($l, $this->collArUserHasPermissions, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUserHasPermissions, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArUserHasPermissions from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArUserHasPermissionsJoinArPermission($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserHasPermissions === null) {
			if ($this->isNew()) {
				$this->collArUserHasPermissions = array();
			} else {

				$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

				$this->collArUserHasPermissions = ArUserHasPermissionPeer::doSelectJoinArPermission($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserHasPermissionPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArUserHasPermissionCriteria) || !$this->lastArUserHasPermissionCriteria->equals($criteria)) {
				$this->collArUserHasPermissions = ArUserHasPermissionPeer::doSelectJoinArPermission($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserHasPermissionCriteria = $criteria;

		return $this->collArUserHasPermissions;
	}

	/**
	 * Clears out the collArReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReports()
	 */
	public function clearArReports()
	{
		$this->collArReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReports collection (array).
	 *
	 * By default this just sets the collArReports collection to an empty array (like clearcollArReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReports()
	{
		$this->collArReports = array();
	}

	/**
	 * Gets an array of ArReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArReports from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReport[]
	 * @throws     PropelException
	 */
	public function getArReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
			   $this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				$this->collArReports = ArReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
					$this->collArReports = ArReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportCriteria = $criteria;
		return $this->collArReports;
	}

	/**
	 * Returns the number of related ArReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReport objects.
	 * @throws     PropelException
	 */
	public function countArReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				$count = ArReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
					$count = ArReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReports);
				}
			} else {
				$count = count($this->collArReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReport object to this object
	 * through the ArReport foreign key attribute.
	 *
	 * @param      ArReport $l ArReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReport(ArReport $l)
	{
		if ($this->collArReports === null) {
			$this->initArReports();
		}
		if (!in_array($l, $this->collArReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReports, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportsJoinArReportSet($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArReportSet($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArReportSet($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportsJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportsJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportsJoinArReportOrderOfChildren($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReports === null) {
			if ($this->isNew()) {
				$this->collArReports = array();
			} else {

				$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

				$this->collArReports = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportCriteria) || !$this->lastArReportCriteria->equals($criteria)) {
				$this->collArReports = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportCriteria = $criteria;

		return $this->collArReports;
	}

	/**
	 * Clears out the collArReportToReads collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportToReads()
	 */
	public function clearArReportToReads()
	{
		$this->collArReportToReads = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportToReads collection (array).
	 *
	 * By default this just sets the collArReportToReads collection to an empty array (like clearcollArReportToReads());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportToReads()
	{
		$this->collArReportToReads = array();
	}

	/**
	 * Gets an array of ArReportToRead objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArReportToReads from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportToRead[]
	 * @throws     PropelException
	 */
	public function getArReportToReads($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
			   $this->collArReportToReads = array();
			} else {

				$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

				ArReportToReadPeer::addSelectColumns($criteria);
				$this->collArReportToReads = ArReportToReadPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

				ArReportToReadPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
					$this->collArReportToReads = ArReportToReadPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportToReadCriteria = $criteria;
		return $this->collArReportToReads;
	}

	/**
	 * Returns the number of related ArReportToRead objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportToRead objects.
	 * @throws     PropelException
	 */
	public function countArReportToReads(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

				$count = ArReportToReadPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
					$count = ArReportToReadPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportToReads);
				}
			} else {
				$count = count($this->collArReportToReads);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportToRead object to this object
	 * through the ArReportToRead foreign key attribute.
	 *
	 * @param      ArReportToRead $l ArReportToRead
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportToRead(ArReportToRead $l)
	{
		if ($this->collArReportToReads === null) {
			$this->initArReportToReads();
		}
		if (!in_array($l, $this->collArReportToReads, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportToReads, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReportToReads from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportToReadsJoinArReport($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
				$this->collArReportToReads = array();
			} else {

				$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

				$this->collArReportToReads = ArReportToReadPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
				$this->collArReportToReads = ArReportToReadPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadCriteria = $criteria;

		return $this->collArReportToReads;
	}

	/**
	 * Clears out the collArReportToReadUserViews collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportToReadUserViews()
	 */
	public function clearArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportToReadUserViews collection (array).
	 *
	 * By default this just sets the collArReportToReadUserViews collection to an empty array (like clearcollArReportToReadUserViews());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = array();
	}

	/**
	 * Gets an array of ArReportToReadUserView objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArReportToReadUserViews from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportToReadUserView[]
	 * @throws     PropelException
	 */
	public function getArReportToReadUserViews($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
			   $this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;
		return $this->collArReportToReadUserViews;
	}

	/**
	 * Returns the number of related ArReportToReadUserView objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportToReadUserView objects.
	 * @throws     PropelException
	 */
	public function countArReportToReadUserViews(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportToReadUserViews);
				}
			} else {
				$count = count($this->collArReportToReadUserViews);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportToReadUserView object to this object
	 * through the ArReportToReadUserView foreign key attribute.
	 *
	 * @param      ArReportToReadUserView $l ArReportToReadUserView
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportToReadUserView(ArReportToReadUserView $l)
	{
		if ($this->collArReportToReadUserViews === null) {
			$this->initArReportToReadUserViews();
		}
		if (!in_array($l, $this->collArReportToReadUserViews, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportToReadUserViews, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportToReadUserViewsJoinArReportToRead($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportToReadUserViewsJoinArReport($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArReportToReadUserViewsJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}

	/**
	 * Clears out the collArUserCanViewReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUserCanViewReports()
	 */
	public function clearArUserCanViewReports()
	{
		$this->collArUserCanViewReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUserCanViewReports collection (array).
	 *
	 * By default this just sets the collArUserCanViewReports collection to an empty array (like clearcollArUserCanViewReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUserCanViewReports()
	{
		$this->collArUserCanViewReports = array();
	}

	/**
	 * Gets an array of ArUserCanViewReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArUser has previously been saved, it will retrieve
	 * related ArUserCanViewReports from storage. If this ArUser is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUserCanViewReport[]
	 * @throws     PropelException
	 */
	public function getArUserCanViewReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
			   $this->collArUserCanViewReports = array();
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

				ArUserCanViewReportPeer::addSelectColumns($criteria);
				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

				ArUserCanViewReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
					$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserCanViewReportCriteria = $criteria;
		return $this->collArUserCanViewReports;
	}

	/**
	 * Returns the number of related ArUserCanViewReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUserCanViewReport objects.
	 * @throws     PropelException
	 */
	public function countArUserCanViewReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

				$count = ArUserCanViewReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

				if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
					$count = ArUserCanViewReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUserCanViewReports);
				}
			} else {
				$count = count($this->collArUserCanViewReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUserCanViewReport object to this object
	 * through the ArUserCanViewReport foreign key attribute.
	 *
	 * @param      ArUserCanViewReport $l ArUserCanViewReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUserCanViewReport(ArUserCanViewReport $l)
	{
		if ($this->collArUserCanViewReports === null) {
			$this->initArUserCanViewReports();
		}
		if (!in_array($l, $this->collArUserCanViewReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUserCanViewReports, $l);
			$l->setArUser($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArUser is new, it will return
	 * an empty collection; or if this ArUser has previously
	 * been saved, it will retrieve related ArUserCanViewReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArUser.
	 */
	public function getArUserCanViewReportsJoinArReport($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArUserPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
				$this->collArUserCanViewReports = array();
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserCanViewReportPeer::AR_USER_ID, $this->id);

			if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelectJoinArReport($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserCanViewReportCriteria = $criteria;

		return $this->collArUserCanViewReports;
	}

	/**
	 * Resets all collections of referencing foreign keys.
	 *
	 * This method is a user-space workaround for PHP's inability to garbage collect objects
	 * with circular references.  This is currently necessary when using Propel in certain
	 * daemon or large-volumne/high-memory operations.
	 *
	 * @param      boolean $deep Whether to also clear the references on all associated objects.
	 */
	public function clearAllReferences($deep = false)
	{
		if ($deep) {
			if ($this->collArUserChangePasswordRequests) {
				foreach ((array) $this->collArUserChangePasswordRequests as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUserHasRoles) {
				foreach ((array) $this->collArUserHasRoles as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUserHasPermissions) {
				foreach ((array) $this->collArUserHasPermissions as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReports) {
				foreach ((array) $this->collArReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportToReads) {
				foreach ((array) $this->collArReportToReads as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportToReadUserViews) {
				foreach ((array) $this->collArReportToReadUserViews as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUserCanViewReports) {
				foreach ((array) $this->collArUserCanViewReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArUserChangePasswordRequests = null;
		$this->collArUserHasRoles = null;
		$this->collArUserHasPermissions = null;
		$this->collArReports = null;
		$this->collArReportToReads = null;
		$this->collArReportToReadUserViews = null;
		$this->collArUserCanViewReports = null;
			$this->aArParty = null;
			$this->aArOrganizationUnit = null;
	}

} // BaseArUser
