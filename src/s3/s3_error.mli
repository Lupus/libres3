(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License version 2 as     *)
(*  published by the Free Software Foundation.                            *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA 02110-1301 USA.                                                    *)
(*                                                                        *)
(*  Special exception for linking this software with OpenSSL:             *)
(*                                                                        *)
(*  In addition, as a special exception, Skylable Ltd. gives permission   *)
(*  to link the code of this program with the OpenSSL library and         *)
(*  distribute linked combinations including the two. You must obey the   *)
(*  GNU General Public License in all respects for all of the code used   *)
(*  other than OpenSSL. You may extend this exception to your version     *)
(*  of the program, but you are not obligated to do so. If you do not     *)
(*  wish to do so, delete this exception statement from your version.     *)
(**************************************************************************)

type t =
  | NoError
  | AccessDenied
  | AccessForbidden
  | AccountProblem
  | AmbiguousGrantByEmailAddress
  | BadDigest
  | BadRequest
  | BucketAlreadyExists
  | BucketAlreadyOwnedByYou
  | BucketNotEmpty
  | CredentialsNotSupported
  | CrossLocationLoggingProhibited
  | EntityTooSmall
  | EntityTooLarge
  | ExpiredToken
  | IllegalVersioningConfigurationException
  | IncompleteBody
  | IncorrectNumberOfFilesInPostRequest
  | InlineDataTooLarge
  | InternalError
  | InvalidAccessKeyId
  | InvalidArgument
  | InvalidBucketName
  | InvalidBucketState
  | InvalidDigest
  | InvalidEncryptionAlgorithmError
  | InvalidLocationConstraint
  | InvalidPart
  | InvalidPartOrder
  | InvalidPayer
  | InvalidPolicyDocument
  | InvalidRange
  | InvalidRequest of string
  | InvalidSecurity
  | InvalidSOAPRequest
  | InvalidStorageClass
  | InvalidTargetBucketForLogging
  | InvalidToken
  | InvalidURI
  | KeyTooLong
  | MalformedACLError
  | MalformedPOSTRequest
  | MalformedXML
  | MaxMessageLengthExceeded
  | MaxPostPreDataLengthExceededError
  | MetadataTooLarge
  | MethodNotAllowed
  | MissingContentLength
  | MissingRequestBodyError
  | MissingSecurityElement
  | MissingSecurityHeader
  | NoLoggingStatusForKey
  | NoSuchBucket
  | NoSuchKey
  | NoSuchLifecycleConfiguration
  | NoSuchCORSConfiguration
  | NoSuchWebsiteConfiguration
  | NoSuchReplicationConfiguration
  | NoSuchTagSetError
  | NoSuchUpload
  | NoSuchVersion
  | NotImplemented
  | NotSignedUp
  | NoSuchBucketPolicy
  | OperationAborted
  | PermanentRedirect
  | PreconditionFailed
  | Redirect
  | RequestIsNotMultiPartContent
  | RequestTimeout
  | RequestTimeTooSkewed
  | RequestTorrentOfBucketError
  | RestoreAlreadyInProgress
  | SignatureDoesNotMatch
  | ServiceUnavailable
  | SlowDown
  | TemporaryRedirect
  | TokenRefreshRequired
  | TooManyBuckets
  | UnexpectedContent
  | UnresolvableGrantByEmailAddress
  | UserKeyMustBeSpecified
  | RemoteServiceUnavailable
  | RemoteServiceTimeout

val info : t -> string * string * Cohttp.Code.status
