using System;
using System.Collections;
using System.Collections.Generic;

namespace NTestCaseBuilder.Examples
{
    internal class IndexedSortedDictionary<TKey, TValue>: IDictionary<TKey, TValue> where TKey: IComparable<TKey>
    {
        #region IDictionary<TKey,TValue> Members

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() { throw new NotImplementedException(); }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

        public void Add(KeyValuePair<TKey, TValue> item) { throw new NotImplementedException(); }

        public void Clear() { throw new NotImplementedException(); }

        public bool Contains(KeyValuePair<TKey, TValue> item) { throw new NotImplementedException(); }

        public void CopyTo
            (KeyValuePair<TKey, TValue>[] array,
             int arrayIndex) { throw new NotImplementedException(); }

        public bool Remove(KeyValuePair<TKey, TValue> item) { throw new NotImplementedException(); }

        public int Count
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        public bool ContainsKey(TKey key) { throw new NotImplementedException(); }

        public void Add
            (TKey key,
             TValue value) { throw new NotImplementedException(); }

        public bool Remove(TKey key) { throw new NotImplementedException(); }

        public bool TryGetValue
            (TKey key,
             out TValue value) { throw new NotImplementedException(); }

        public TValue this[TKey key]
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public ICollection<TKey> Keys
        {
            get { throw new NotImplementedException(); }
        }

        public ICollection<TValue> Values
        {
            get { throw new NotImplementedException(); }
        }

        #endregion

        public TValue this[int index]
        {
            get { throw new NotImplementedException();}
        }
    }
}